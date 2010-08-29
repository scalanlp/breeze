/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
package scalanlp.distributed;

import java.net.URI;

import scala.collection.generic.{GenericCompanion,CanBuildFrom}
import scala.collection.mutable.Builder;

import scalanlp.collection.{LazyIterable,LazyIterableLike};
import scalanlp.serialization.DataSerialization;

import SocketService.reply;


/**
 * Paired server side and client side proxies for a remote view of an
 * iterator.  Instances should be created via a RemoteIterable.
 *
 * @author dramage
 */
object RemoteIterator {
  protected object Messages {
    case object HasNext;
    case object Next;
  }

  import Messages._;

  private var _uid = 0l;
  private def nextUID : Long = synchronized {
    _uid += 1;
    _uid - 1;
  }

  import java.net._;
  import java.io.{DataInputStream,DataOutputStream};

  class Service[V](iterator : Iterator[V])(implicit writer : DataSerialization.Writable[V])
  extends Threadable {
    val port = SocketUtils.freePort;
    val listener = new ServerSocket(port);

    val uri = URI.create("socket://"+SocketUtils.hostName+":"+port);

    def run() {
      val socket = listener.accept();
      val out = new DataOutputStream(socket.getOutputStream);
      for (value <- iterator) {
        out.writeBoolean(true);
        writer.write(out, value)
      }
      out.writeBoolean(false);
      out.close();
      socket.close();
    }
  }

  class Client[V](uri : URI)(implicit reader : DataSerialization.Readable[V])
  extends Iterator[V] {
    require(uri.getScheme == "socket", "Must be socket://");
    require(uri.getPath == "", "Can not have path.");

    protected lazy val socket = {
      val s = new Socket(uri.getHost, uri.getPort);
      s.setSoTimeout(0);
      s
    }

    /** streams - NB always create out before in! */
    lazy val input =
      new DataInputStream(socket.getInputStream);

    var knowHasNext = false;
    var currHasNext = false;

    override def hasNext = {
      if (!knowHasNext) {
        knowHasNext = true;
        currHasNext = input.readBoolean;
        if (!currHasNext) {
          // no more - close everything down
          input.close;
          socket.close;
        }
      }
      currHasNext;
    }

    override def next = {
      if (!hasNext) {
        throw new NoSuchElementException();
      }
      knowHasNext = false;
      reader.read(input);
    }
  }
}

trait RemoteIterableLike[A,+This<:RemoteIterable[A] with RemoteIterableLike[A,This]]
extends LazyIterable[A] with LazyIterableLike[A,This] { self =>
  // server-side mapping
  val client : SocketClient;

  implicit val readable : DataSerialization.Readable[A];

  import RemoteIterable.Messages._;

  trait Transformed[B] extends super.Transformed[B] with RemoteIterable[B] {
    val client = self.client;
    val readable = format;

    protected[this] implicit val format : DataSerialization.ReadWritable[B];
  }

  trait Mapped[B] extends super.Mapped[B] with Transformed[B] {
    protected[this] val mapping: A => B;
    override def iterator = {
      val uri = (client !? MsgMappedIterator(mapping, format)).asInstanceOf[URI];
      new RemoteIterator.Client[B](uri);
    }
  }

  override def size =
    (client !? MsgSize).asInstanceOf[Int];

  override def iterator = {
    val uri = (client !? MsgIterator).asInstanceOf[URI];
    new RemoteIterator.Client[A](uri);
  }

  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That = {
    require(bf.isInstanceOf[CanBuildReadWritableFrom[_,B,_]],
            "Must cast Iterable to DistributedIterable before calling map.")
    (new Mapped[B] {
        val mapping = f;
        val format = bf.asInstanceOf[CanBuildReadWritableFrom[_,B,_]].format;
     }).asInstanceOf[That];
  }

  override def stringPrefix = "RemoteIterable";

}

trait RemoteIterable[A] extends RemoteIterableLike[A,RemoteIterable[A]] {
  override def companion = RemoteIterable;
}


/**
 * Paired server side and client side proxies for a remote view of an
 * iterable.  Use service() to host an iterable, which must be run by the
 * caller.  Use client() to connect to a remote.
 *
 * @author dramage
 */
object RemoteIterable extends GenericCompanion[RemoteIterable] {

  protected[distributed] object Messages {
    case object MsgSize;
    case object MsgIterator;
    case class MsgMappedIterator[A,B](fn : (A=>B), writer : DataSerialization.Writable[B]);
  }

  import Messages._;

  private var _uid = 0l;
  private def nextUID : Long = synchronized {
    _uid += 1;
    _uid - 1;
  }

  class Service[V:DataSerialization.Writable](iterable : Iterable[V], dispatch : SocketDispatch = SocketDispatch())
  extends SocketService("/iterable/"+nextUID, dispatch) {
    override def react = synchronized {
      case MsgIterator =>
        val service = new RemoteIterator.Service[V](iterable.iterator);
        service.runAsDaemon;
        reply { service.uri; }

      case MsgSize =>
        reply { iterable.size; }

      case MsgMappedIterator(fn, writer) =>
        val service = new RemoteIterator.Service(iterable.iterator.map(fn))(writer)
        service.runAsDaemon;
        reply { service.uri; }

      case _ =>
        throw new RuntimeException();
    }
  }

  protected val noBuilder = new scala.collection.TraversableView.NoBuilder();

  override def newBuilder[A]: Builder[A, RemoteIterable[A]] =
    noBuilder.asInstanceOf[Builder[A,RemoteIterable[A]]];

  implicit def canBuildFrom[A:DataSerialization.ReadWritable] = {
    new CanBuildReadWritableFrom[RemoteIterable[_],A,RemoteIterable[A]] {
      override val format = implicitly[DataSerialization.ReadWritable[A]];
      override val inner = new CanBuildFrom[RemoteIterable[_],A,RemoteIterable[A]] {
        override def apply(from: RemoteIterable[_]) =
          noBuilder.asInstanceOf[Builder[A,RemoteIterable[A]]];
        override def apply() =
          noBuilder.asInstanceOf[Builder[A,RemoteIterable[A]]];
      }
    }
  }

  def service[V:DataSerialization.Writable](iterable : Iterable[V], dispatch : SocketDispatch = SocketDispatch()) =
    new Service[V](iterable);

  def client[V](uri : URI)(implicit format : DataSerialization.Readable[V]) : RemoteIterable[V] = {
    new RemoteIterable[V]() {
      val client = SocketClient(uri);
      val readable = format;
    }
  }
}

trait CanBuildReadWritableFrom[-From,Elem,+To] extends CanBuildFrom[From,Elem,To] {
  val inner : CanBuildFrom[From,Elem,To];
  val format : DataSerialization.ReadWritable[Elem];

  override def apply() =
    inner.apply();

  override def apply(from : From) =
    inner.apply(from);
}

//object CanBuildReadWritableFrom {
//  implicit def canBuildReadWritableFrom[From,Elem,To]
//  (implicit bf : CanBuildFrom[From,Elem,To], rw : DataSerialization.ReadWritable[Elem])
//  : CanBuildReadWritableFrom[From,Elem,To] = new CanBuildReadWritableFrom[From,Elem,To] {
//    override val inner = bf;
//    override val format = rw;
//  }
//}
