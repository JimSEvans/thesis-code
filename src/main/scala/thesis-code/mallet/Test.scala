package thesiscode.dataprocessing

import java.io.{File,FileWriter,PrintWriter}
import scala.io.Source
import thesiscode.util.Util._
import scala.math._


object Test {


	def kl(p: Seq[Double], q: Seq[Double]) = 
  		(p zip q).map { case (a, b) => (log(a/b)*a) }.sum
  	
	def js(p: Seq[Double], q: Seq[Double]) = {
    
	  	val m = (p zip q).map { case (a, b) => ((0.5)*(a+b)) }

	  	
	    (0.5)*kl(p, m)+(0.5)*kl(q, m)
	    
    }




	val testing = new File("data/mallet/3topic-word-weights.txt")

	// val scDir = new File("data/mallet/sc-20topic-word-counts.txt")
	val psDir = new File("data/mallet/ps-20topic-word-counts.txt")
	val scDir = new File("data/mallet/3topic-word-weights.txt")
	// val psDir = new File("data/mallet/3topic-word-counts.txt")


	val lines: Vector[String] = Source.fromFile(testing).getLines.toVector
		.filter(_ != "")

	val scLines: Vector[String] = Source.fromFile(scDir).getLines.toVector
		.filter(_ != "")
	val psLines: Vector[String] = Source.fromFile(psDir).getLines.toVector
		.filter(_ != "")

// // println("got lines")

// 	val scVocab: Set[String] = scLines.filter(_.startsWith("0"))
// 		.map(l => {
// 			// println(l)
// 			l.split("\\s+")(1)})
// 		.toSet
// 	val psVocab: Set[String] = psLines.filter(_.startsWith("0"))
// 		.map(l => {
// 			// println(l)
// 			l.split("\\s+")(1)})
// 		.toSet

// println("got vocabs")

// 	val scOnly = (scVocab -- psVocab).toVector
// 	val psOnly = (psVocab -- scVocab).toVector

// println("got uniq vocabs")


	// val boo = (scVocab ++ psOnly).toVector.sorted == (psVocab ++ scOnly).toVector.sorted


println("getting vectors of topics...")


	def getSmoothedTopicsWriteToFile(
		topicWordWeightLines: Vector[String] 
		// addForSmoothing: Vector[String] 
		//writeToDirName: String // either "sc" or "ps"
		):
		//Iterator[Vector[Double]] = {
		Unit = {


		val addForSmoothing = Vector[String]()//if (topicWordWeightLines == scLines) psOnly else if (topicWordWeightLines == psLines) scOnly else {println("ERROR");Vector("WRONG")}

		val writeToDirName = if (topicWordWeightLines == scLines) "sc" else if (topicWordWeightLines == psLines) "ps" else {println("ERROR");"WRONG"}

		val topicIds: Iterator[String] = topicWordWeightLines.map(line=> line.split("\\s+")(0).toInt)
			.toSet
			.toVector
			.sorted
			.map(x=>x.toString)
			.toIterator

		val add = addForSmoothing.map{ word =>
			"\t" + word + "\t" + "0.01"
		}

		println("got words for smoothing")

		val itOfTopicVectorsRaw: Iterator[Vector[String]] = topicIds.map{ topicId => 
				// println(topicId.toString)
				(topicWordWeightLines.filter(l => l.split("\\s+")(0) == topicId) ++ (add.map(str => topicId + str)))
					.sortBy(l => l.split("\\s+")(1))
			}
	
		println("got it[vect[String]]")

		val itOfTopicVectors: Iterator[Vector[(String,String)]] = itOfTopicVectorsRaw
			.map{topicVector => 
				topicVector.map{ wordAndWeightString =>
					val wordAndWeight = wordAndWeightString.split("\\s+").drop(1)
					(wordAndWeight(0), wordAndWeight(1))
				}
			}

		println("got it[vect[tupls]]")
		

		val itOfTopicVectorsNormalized = itOfTopicVectors
			.map{ topicVector => 
				val weightTotal = topicVector.map(_._2.toDouble).sum
				//println(weightTotal)
				weightTotal
			}

		itOfTopicVectorsNormalized.foreach(println)

	}
	






	def main(args: Array[String]):Unit = {


		getSmoothedTopicsWriteToFile(scLines)

		println("\n\n\n\n")

		getSmoothedTopicsWriteToFile(psLines)

	}//main

}

