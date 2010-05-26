package scalanlp.graphs

/*
 Copyright 2010 David Hall, Daniel Ramage

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

trait Graph {
  type Node;
  type Edge;

  def edges: Iterator[Edge];
  def endpoints(e: Edge):(Node,Node);
  def nodes: Iterator[Node];
  def edgesTouching(n: Node): Iterator[Edge];
  def neighbors(n: Node): Iterator[Node]
  def getEdge(n1: Node, n2: Node): Option[Edge];
}

trait Digraph extends Graph {
  def source(e: Edge): Node
  def sink(e: Edge): Node

  def edgesFrom(n: Node): Iterator[Edge];
  def edgesTo(n: Node): Iterator[Edge];
  def edgesTouching(n: Node) = edgesFrom(n) ++ edgesTo(n);

}
