package breeze.optimize.linear

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

/**
 * Algorithms for finding a bipartite matching.
 * We include one optimal algorithm (KuhnMunkres)
 * and one greedy algorithm (competitive linking).
 *
 * Algorithms find <b> minimum </b> matchings.
 *
 * @author dlwh
 */
trait BipartiteMatching {
  def extractMatching(weights: Seq[Seq[Double]]): (Seq[Int], Double);
}
