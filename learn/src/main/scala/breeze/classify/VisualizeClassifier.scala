package breeze.classify

/**
 * 
 * @author dlwh

object VisualizeClassifier {
  def visualize(trainer: Classifier.Trainer[Boolean,DenseVector[Double]],
                data: Iterable[Example[Boolean,DenseVector[Double]]],
                npoints: Int = 10000) = {
    require(data.head.features.size == 3)
    val classifier = trainer.train(data)
    val xmin = data.map(_.features(1)).min
    val xmax = data.map(_.features(1)).max
    val ymin = data.map(_.features(2)).min
    val ymax = data.map(_.features(2)).max
    val points = for( v <- new HaltonSequence(2).sample(npoints)) yield DenseVector(1.0, xmin + (xmax - xmin) * v.data(0), ymin + (ymax - ymin) * v.data(1))
    val x = new DenseVectorCol(points.map(_ apply 1).toArray)
    val y = new DenseVectorCol(points.map(_ apply 2).toArray)
    val classes = points.map(classifier.map{ case true => "+";  case false => "-"}).toIndexedSeq
    val pf : PartialFunction[Int,String] = { case x => classes(x)}
    val colorsPF: PartialFunction[Int, Color] = {
      case i => if(classifier(points(i))) Color.RED else Color.BLUE
    }
    Plotting.scatter(x,y,DenseVector.ones[Double](x.size) * .002, colorsPF, labels = pf)
  }

} */

