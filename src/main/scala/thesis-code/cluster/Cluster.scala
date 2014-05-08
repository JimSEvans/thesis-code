// package.classify

// /**
//   * An example of using the API to cluster the documents in the well-known 20
//   * news groups data. See the classification example above for more details
//   * about the corpus.
//   *
//   * Example command line to get 10 clusters using euclidean distance and
//   * requiring each word/feature to have been seen 20 times or more.
//   * 
//   * $ nak run nak.example.TwentyNewsGroupsKmeansExample -k 20 -d e -c 20 20news/20news-bydate-train
//   */

// object ClusterDocs {
// 	import nak.NakContext._
// 	import nak.core._
// 	import nak.data._
// 	import nak.cluster._
// 	import nak.util.ConfusionMatrix
// 	import breeze.linalg.SparseVector
// 	import java.io.File


// 	def main(args: Array[String]) {

// 		val opts = ClusteringOpts(args)

// 		println("Processing files... ")
// 	    val trainDir = new File(opts.dirname())
// 	    val trainingExamples = fromLabeledDirs(trainDir).toList
// 	    val indexer = new ExampleIndexer(false)
// 	    val batchFeaturizer = new TfidfBatchFeaturizer[String](opts.cutoff())
// 	    val examples = batchFeaturizer(trainingExamples).map(indexer)
// 	    val (lmap,fmap) = indexer.getMaps
// 	    val numFeatures = fmap.size
// 	    val sparseExamples = examples.toIndexedSeq.map { example =>
// 	      example.map { features =>
// 	        SparseVector(numFeatures)(condense(features).map(_.tuple): _*)
// 	      }.features
// 	    }

// 	}

//   /**
//    * An object that sets up the configuration for command-line options using
//    * Scallop and returns the options, ready for use.
//    */
//   object ClusteringOpts {

//     import org.rogach.scallop._
  
//     def apply(args: Array[String]) = new ScallopConf(args) {
//       banner("""
//              For usage see below:
//              """)
//       val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
//       val cutoff = opt[Int]("cutoff", default=Some(2), validate = (0<),
//         descr="A cutoff specifying the minumum number of times a feature must be observed.")
//       val distanceFunctions = Set("c","cosine","e","euclidean","m","manhattan")
//       val distance = opt[String]("dist", default=Some("cosine"), validate = distanceFunctions,
//         descr = "The distance function to use. Possible values: " + distanceFunctions.toSeq.sorted.mkString(",") )

//       val k = opt[Int]("num-clusters",short='k', required=true, validate = (0<), descr="The number of clusters to find.")
//       val verbose = opt[Boolean]("verbose")
//       val dirname = trailArg[String]("dirname", descr = "The input directory.")
//     }
//   }
// }


// object TwentyNewsGroupsKmeansExample {

//   import nak.NakContext._
//   import nak.core._
//   import nak.data._
//   import nak.cluster._
//   import nak.util.ConfusionMatrix
//   import breeze.linalg.SparseVector
  
//   import java.io.File

//   def main(args: Array[String]) {
//     val opts = ClusteringOpts(args)

//     // We need this codec for reading in the 20 news groups files.
//     implicit val isoCodec = scala.io.Codec("ISO-8859-1")

//     println("Processing files... ")
//     val trainDir = new File(opts.dirname())
//     val trainingExamples = fromLabeledDirs(trainDir).toList
//     val indexer = new ExampleIndexer(false)
//     val batchFeaturizer = new TfidfBatchFeaturizer[String](opts.cutoff())
//     val examples = batchFeaturizer(trainingExamples).map(indexer)
//     val (lmap,fmap) = indexer.getMaps
//     val numFeatures = fmap.size
//     val sparseExamples = examples.toIndexedSeq.map { example =>
//       example.map { features =>
//         SparseVector(numFeatures)(condense(features).map(_.tuple): _*)
//       }.features
//     }

//     val distanceFun = opts.distance() match {
//       case "c" | "cosine" => Kmeans.cosineDistance
//       case "m" | "manhattan" => Kmeans.manhattanDistance
//       case "e" | "euclidean" => Kmeans.euclideanDistance
//     }
    
//     val kmeans = new Kmeans[SparseVector[Double]](
//       sparseExamples,
//       distanceFun,
//       minChangeInDispersion=0.001,
//       maxIterations=10
//     )

//     println("Clustering... ")
//     val (dispersion, centroids) = kmeans.run(opts.k(),1)
//     if (opts.verbose()) {
//       println(s"Dispersion: $dispersion")
//       println("Centroids:")
//       centroids.foreach(c=>println(c(0 until 5).toArray.mkString(" ")))
//     }

//     println("Evaluating...")
//     val goldLabels = examples.map(_.label)
//     val (distance, predictions) = kmeans.computeClusterMemberships(centroids)
//     val inputs = examples.map(_.features)
//     val cmatrix = ConfusionMatrix(goldLabels, predictions, inputs)
//     println(cmatrix)
//     if (opts.verbose())
//       println(lmap.toSeq.sortBy(_._2).foreach(println))
//   }

//   /**
//    * An object that sets up the configuration for command-line options using
//    * Scallop and returns the options, ready for use.
//    */
//   object ClusteringOpts {

//     import org.rogach.scallop._
  
//     def apply(args: Array[String]) = new ScallopConf(args) {
//       banner("""
//              For usage see below:
//              """)
//       val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
//       val cutoff = opt[Int]("cutoff", default=Some(2), validate = (0<),
//         descr="A cutoff specifying the minumum number of times a feature must be observed.")
//       val distanceFunctions = Set("c","cosine","e","euclidean","m","manhattan")
//       val distance = opt[String]("dist", default=Some("cosine"), validate = distanceFunctions,
//         descr = "The distance function to use. Possible values: " + distanceFunctions.toSeq.sorted.mkString(",") )

//       val k = opt[Int]("num-clusters",short='k', required=true, validate = (0<), descr="The number of clusters to find.")
//       val verbose = opt[Boolean]("verbose")
//       val dirname = trailArg[String]("dirname", descr = "The input directory.")
//     }
//   }
  
// }
