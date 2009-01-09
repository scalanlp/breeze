import java.io._;
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
