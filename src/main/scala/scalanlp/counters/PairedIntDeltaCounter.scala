package scalanlp.counters;

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

import scala.collection.mutable.HashMap;

/**
* Uses a pre-existing counter for default values. 
* Any updates to the first counter may or may not be reflected.
* @author(dlwh)
*/
@serializable
class PairedIntDeltaCounter[K1,K2](origin : PairedIntCounter[K1,K2]) extends PairedIntCounter[K1,K2] {
  // may be overridden with other counters
  override def default(k1 : K1) :IntCounter[K2] = new IntDeltaCounter[K2](origin(k1));
}

