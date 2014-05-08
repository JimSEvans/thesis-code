package thesiscode.dataprocessing

import java.io.{File,FileWriter,PrintWriter}
import scala.io.Source
import thesiscode.util.Util._
import scala.math._


object Divergences {


	def kl(p: Seq[Double], q: Seq[Double]) = 
  		(p zip q).map { case (a, b) => (log(a/b)*a) }.sum
  	
	def js(p: Seq[Double], q: Seq[Double]) = {
    
	  	val m = (p zip q).map { case (a, b) => ((0.5)*(a+b)) }

	  	
	    (0.5)*kl(p, m)+(0.5)*kl(q, m)
	    
    }

def getDivergences() = {

	val scDir = new File("data/mallet/sc")
	val scTopicFiles = scDir.listFiles.toVector.filterNot(_.isHidden).sortBy(x => x.getName.drop(5).dropRight(4).toInt)
	//scTopicFiles.foreach(f=>println(f.getName))


	val psDir = new File("data/mallet/ps")
	val psTopicFiles = psDir.listFiles.toVector.filterNot(_.isHidden).sortBy(x => x.getName.drop(5).dropRight(4).toInt)
	//psTopicFiles.foreach(f=>println(f.getName))


	// print("[")

	val matrix:Vector[Vector[Long]] =
		scTopicFiles.map{ scTopicFile =>
			print("[")
			val scIt = Source.fromFile(scTopicFile).getLines
			scIt.next
			val scVector = scIt.map(line => line.split("\\s+")(1).toDouble).toVector
			val row = psTopicFiles.map { psTopicFile =>
				val psIt = Source.fromFile(psTopicFile).getLines
				psIt.next
				val psVector = psIt.map(line => line.split("\\s+")(1).toDouble).toVector
				val jd = js(scVector, psVector)
				val divergence = if (jd == 0.0) 0 else (10000000000000000L * jd).toLong
				print(divergence + ", ")
				divergence
			}.toVector
			println("]")
			row
		}.toVector

	println("]")


}

def main(args: Array[String]):Unit = {


		getDivergences()	
	}

}



object smoothTopics {



// def foo () = {


	//val testing = new File("data/mallet/3topic-word-counts.txt")

	val scFile = new File("data/mallet/sc30-topic-word-weights.txt")
	val psFile = new File("data/mallet/ps30-topic-word-weights.txt")


	//val lines: Vector[String] = Source.fromFile(testing).getLines.toVector
		//.filter(_ != "")

	val scLines: Vector[String] = Source.fromFile(scFile).getLines.toVector
		.filter(_ != "")
	val psLines: Vector[String] = Source.fromFile(psFile).getLines.toVector
		.filter(_ != "")

// println("got lines")

	val scVocab: Set[String] = scLines.filter(_.startsWith("0"))
		.map(l => {
			// println(l)
			l.split("\\s+")(1)})
		.toSet
	val psVocab: Set[String] = psLines.filter(_.startsWith("0"))
		.map(l => {
			// println(l)
			l.split("\\s+")(1)})
		.toSet

println("got vocabs")

	val scOnly = (scVocab -- psVocab).toVector
	val psOnly = (psVocab -- scVocab).toVector

// println("got uniq vocabs")


	// val boo = (scVocab ++ psOnly).toVector.sorted == (psVocab ++ scOnly).toVector.sorted



	def getSmoothedTopicsWriteToFile(
		topicWordWeightLines: Vector[String] 
		// addForSmoothing: Vector[String] 
		//writeToDirName: String // either "sc" or "ps"
		):
		//Iterator[Vector[Double]] = {
		Unit = {


		val addForSmoothing = if (topicWordWeightLines == scLines) psOnly else if (topicWordWeightLines == psLines) scOnly else {println("ERROR");Vector("WRONG")}

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
		

		val itOfTuplesNormalizedAndWeight = itOfTopicVectors
			.map{ topicVector => 
				val weightTotal = topicVector.map(_._2.toDouble).sum
				val newVect = topicVector.map{ tupl => 
					(tupl._1, tupl._2.toDouble/weightTotal, weightTotal)
				}
				(newVect, weightTotal)
			}

		println("got normalized it[vect[tupls]]")

		// val itOfProbabilityVectors = {
		// 	itOfTopicVectorsNormalized.map{ tuplVector =>
		// 		tuplVector.map(tupl => (tupl._2, weight))
		// 	}
		// }

		// println("got probabilities: it[vect[Double]]")


	var topicNum = 0

	itOfTuplesNormalizedAndWeight.foreach{ vectWeightTupl =>
		val f = new File("data/mallet/" + writeToDirName + "/" + "topic" + topicNum + ".txt")
		printToFile(f)(p => {
			p.println(vectWeightTupl._2)
			vectWeightTupl._1.foreach(tupl => p.println(tupl._1 + " " + tupl._2))
			})
		topicNum += 1
		}
	}
	

	// def printMatrix(
	// 	// addForSmoothing: Vector[String] 
	// 	//writeToDirName: String // either "sc" or "ps"
	// 	):
	// 	//Iterator[Vector[Double]] = {
	// 	Unit = {

	// 	val scDir = new File("data/mallet/sc")
	// 	val psDir = new File("data/mallet/ps")

	// 	val scFiles = scDir.listFiles.toVector
	// 	val psFiles = psDir.listFiles.toVector




	// }
	// }


	def main(args: Array[String]):Unit = {


		//getSmoothedTopicsWriteToFile(scLines)
		
		//getSmoothedTopicsWriteToFile(psLines)


		//getDivergences()	


		// val scTopics = getSmoothedTopics(scLines, psOnly)

		// var psTopics = Iterator[Vector[Double]]()

		// val matrix:Vector[Vector[Double]] =
		// 	scTopics.map{ scTopic =>
		// 		println("\nWorking on a row of JS Divergences..")
		// 		psTopics = getSmoothedTopics(psLines, scOnly)
		// 		val row = psTopics.map { psTopic =>
		// 			val jd = js(scTopic, psTopic)
		// 			println("got a divergence!")
		// 			if (jd == 0.0) 0.000000001 else jd
		// 			// println(
		// 			// 	(scTopics.indexOf(scTopic), psTopics.indexOf(psTopic)) + " " + js(scTopic, psTopic)
		// 			// 		)
		// 		}.toVector
		// 		println("Finished a row!!!!!!")
		// 		row
		// 	}.toVector

		// // val foo:Vector[Vector[String]] = {
		// // it.toVector.map{ vect1 =>
		// // 	it2.toVector.map { vect2 =>
		// // 		vect1(0)+vect2(0)
		// // 		}
		// // 	}
		// // }


		// print("[")

		// val nonLastRows = matrix.dropRight(1)
		// val lastRow = matrix.last

		// nonLastRows.foreach{ vectorOfDivergences => 
		// 	val nonLast = vectorOfDivergences.dropRight(1)
		// 	val last = vectorOfDivergences.last
		// 	print("[")
		// 	nonLast.foreach{ divergence => print(divergence + ", ") }
		// 	print(last)
		// 	println("],")
		// }

		// val lastRowNonLast = lastRow.dropRight(1)
		// val lastRowLast = lastRow.last
		// print("[")
		// lastRowNonLast.foreach{ divergence => print(divergence + ", ") }
		// print(lastRowLast)
		// print("]")
		// println("]")

		// // println(scTopics(0)(0))
		// // println(psTopics(0)(0))


	}//main

}

