import java.io._;

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

import scalanlp.data._;
import scalanlp.classify._;

val trainData = 
  for( dir <- new File(args(0)).listFiles;
  file <- dir.listFiles)
  yield Bag.fromFile(file);

val testData = for( dir <- new File(args(1)).listFiles;
  file <- dir.listFiles)
  yield Bag.fromFile(file);

val nb = new NaiveBayes(trainData,3,0.1);

val trainRight = for(ex <- trainData;
  label = nb(ex))
 yield if(label == ex.label) 1.0 else 0.0;

val trainAcc = trainRight.reduceLeft(_+_) / trainData.size;
  
val testRight = for(ex <- testData;
  label = nb(ex))
 yield if(label == ex.label) 1.0 else 0.0;

val testAcc = testRight.reduceLeft(_+_) / testData.size;
  
println(trainAcc + " " + testAcc);
