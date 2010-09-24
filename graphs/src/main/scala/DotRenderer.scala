package scalanlp.graphs

/*
 Copyright 2010 David Hall

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


/**
 * Renders graphs in Dot Format
 * @author dlwh
 */
object DotRenderer {
  /**
   * Renders the graph using the dot language.
   *
   * @param g the graph
   * @param directed whether or not to use a "digraph" or a normal graph.
   * @param nodeValue The variable name to give to nodes in the graph. default is .toString
   * @param nodeLabeler the label of the nodes, distinct from the variable name. default is toString
   * @param edgeLabeler the label for any edge. default is empty string.
   */
  def render[Node,Edge](g: Graph[Node,Edge],
                        directed: Boolean = false,
                        nodeValue: (Node=>String)=((n: Node)=>n.toString),
                        nodeLabeler:(Node=>String)=((n:Node) =>n.toString),
                        edgeLabeler:(Edge=>String)=((_:Edge) =>"") ):String = {
    def escape(s: String) = s.replaceAll("\"","\\\"");

    val sb = new StringBuilder;
    val edgeMarker = if(directed) "->" else "--";
    if(directed)
      sb ++= "digraph A {\n";
    else
      sb ++= "graph A {\n";

    val states = collection.mutable.Set[Node]();
    g.edges.foreach { e =>
      val (src,sink) = g.endpoints(e);
      sb ++= "    \"" + escape(nodeValue(src)) + "\" "+edgeMarker +" \"" + escape(nodeValue(sink)) +"\"";
      val edgeLabel = escape(edgeLabeler(e))
      if(!edgeLabel.isEmpty)
        sb ++= "[ label=\""+edgeLabel + "\"]";
      sb ++= "\n";
      states += src;
      states += sink;
    }

    for(s <- states) {
      sb ++= "    \"" + escape(nodeValue(s)) + "\"";
      sb ++= "[ label=\""+ escape(nodeLabeler(s)) + "\"]\n";
    }
    sb ++= "}";
    sb.toString;
  }

}