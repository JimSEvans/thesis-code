package thesiscode.dataprocessing

import thesiscode.util.Util._
import java.io.{File,FileWriter,PrintWriter}
import scala.sys.process._
import scala.util.Random.shuffle
import scala.io.Source


object ListBasedTrainFileList {

	val dir = new File("data/corpusJSON/listBasedList/main/")

	val files = dir.listFiles.toVector

	var scFiles = List[File]()

	var psFiles = List[File]()

	val r = new scala.util.Random(1)

	files.foreach{ file =>
		if (getFirstLine(file).startsWith("sc")) scFiles = file::scFiles
		else psFiles = file::psFiles
	}

	println(scFiles.length)
	println(psFiles.length)

	val scPlainHalf = scFiles.take(461).toVector

	val psPlainHalf = psFiles.take(461).toVector

	val smallPlainTrainingSet = scPlainHalf ++ psPlainHalf

	val smallPlainFile = new File("data/corpusJSON/dataSplitsDir/listBased/smallPlain.txt")

	val scShuffled = r.shuffle(scFiles).toVector

	val psShuffled = r.shuffle(psFiles).toVector

	val scRandomHalf = scShuffled.take(461)

	val psRandomHalf = psShuffled.take(461)

	val smallRandomTrainingSet = scRandomHalf ++ psRandomHalf

	val smallRandomFile = new File("data/corpusJSON/dataSplitsDir/listBased/smallRandom.txt")

	printToFile(smallPlainFile)(p => {
		smallPlainTrainingSet.foreach(x=> p.println(x))
		})

	printToFile(smallRandomFile)(p => {
		smallRandomTrainingSet.foreach(x=> p.println(x))
		})

	val bigTrainingSet = files

	val bigFile = new File("data/corpusJSON/dataSplitsDir/listBased/big.txt")


	printToFile(bigFile)(p => {
		bigTrainingSet.foreach(x=> p.println(x.getPath))
		})


	def main(args: Array[String]):Unit = {


	}//main


}











class DocumentOrganizer(
	lastTwoDirs: String = "mainList/main"
	) {

	val dataSetID = lastTwoDirs.split("/")(0).dropRight(4) // e.g. oldList/main => old

	val corpusJsonPath = "data/corpusJSON/"
	val corpusDir = new File(corpusJsonPath + lastTwoDirs)
	val files: Array[File] = corpusDir.listFiles.filter(_.isFile).filterNot(_.isHidden)

	val dataSplitsDirPath = corpusJsonPath + "dataSplitsDir/"

	// if (dataSplitsDir.exists) {
	// 	if (dataSplitsDir.listFiles.filterNot(_.isHidden).isEmpty == false) {
	// 		throw new java.lang.Exception("dataSplits directory already contains files or directories.")
	// 	}
	// } else {
	// 	dataSplitsDir.mkdir
	// }


	def getFirstLine(f: File): String = {
	  Source.fromFile(f).getLines.next
	  }

	val fileTuples = files.map{file =>
		(file.getPath, getFirstLine(file))
	}
	//fileTuples.foreach(println)
	//throw new java.lang.Exception

	//create empty lists divided by both topic and perspective
	var scPara = List[(String, String)]()
	var psPara = List[(String, String)]()
	var scHealth = List[(String, String)]()
	var psHealth = List[(String, String)]()
	var scRelig = List[(String, String)]()
	var psRelig = List[(String, String)]()
	var scOther = List[(String, String)]()
	var psOther = List[(String, String)]()

	// //add files (as tuples) to appropriate lists
	// fileTuples.foreach{tupl =>
	// 	val l = tupl._2
	// 	if (l.contains("para")||l.contains("ghost")||l.contains("psychic")||l.matches(".*astro\\W.*")||l.contains("tarot")) {
	// 		if (l.startsWith("sc")) {scPara = tupl::scPara} else {psPara = tupl::psPara}
	// 	}
	// 	else if (l.startsWith("sc") && l.contains("relig")) {scRelig = tupl::scRelig}
	// 	else if (l.startsWith("ps") && l.contains("health")) {psHealth = tupl::psHealth}
	// 	else if (l.contains("relig")) {psRelig = tupl::psRelig}
	// 	else if (l.contains("health")) {scHealth = tupl::scHealth}
	// 	else if (l.startsWith("sc")) {scOther = tupl::scOther}
	// 	else {psOther = tupl::psOther}
	// }

//add files (as tuples) to appropriate lists
	fileTuples.foreach{tupl =>
		val l = tupl._2
		if (l.contains("para")||l.contains("ghost")||l.contains("psychic")||l.matches(".*astro\\W.*")||l.contains("tarot")||l.contains("hypno")) {
			if (l.startsWith("sc")) {scPara = tupl::scPara} else {psPara = tupl::psPara}
		}
		else if (l.startsWith("sc") && l.contains("relig")) {scRelig = tupl::scRelig}
		else if (l.startsWith("ps") && (l.contains("health")||l.contains("heatlh"))) {psHealth = tupl::psHealth}
		else if (l.contains("relig")) {psRelig = tupl::psRelig}
		else if (l.contains("health")) {scHealth = tupl::scHealth}
		else if (l.startsWith("sc")) {scOther = tupl::scOther}
		else {psOther = tupl::psOther}
	}

	// splits a list of documents (as tuples) into a train, dev, and test set approximately 70:15:15
	def splitIntoSets (tuples: List[(String, String)]): List[List[(String, String)]] = {
		val smallSetSize = Math.round(0.15*(tuples.length)).toInt
		val randomized = scala.util.Random.shuffle(tuples)
		val testSet = randomized.slice(0, smallSetSize)
		val devSet = randomized.slice(smallSetSize, smallSetSize*2)
		val trainSet = randomized.drop(smallSetSize*2)
		List(trainSet, devSet, testSet)
	}

	val mainList: List[List[(String,String)]] = List(scPara,psPara,scHealth,psHealth,scRelig,psRelig,scOther,psOther)

	val listOfTopicPerspectiveLists: List[List[List[(String,String)]]] = mainList.map(splitIntoSets)

	val trainSets: List[List[(String, String)]] = listOfTopicPerspectiveLists.map(_(0))
	val devSets = listOfTopicPerspectiveLists.map(_(1))
	val testSets = listOfTopicPerspectiveLists.map(_(2))



	val trainSet = trainSets.flatten
	val devSet = devSets.flatten
	val testSet = testSets.flatten

	val tFile = new File ("data/corpusJSON")

	// val trainSet = Source.fromFile("data/corpusJSON/dataSplits_trainDevTest/train/all.txt").getLines
	// 	.toList
	// 	.map{ l =>
	// 		val a = l.split(" ")
	// 		(a(0), a(1))
	// 	}

	// val devSet = Source.fromFile("data/corpusJSON/dataSplits_trainDevTest/dev/all.txt").getLines
	// 	.toList
	// 	.map{ l =>
	// 		val a = l.split(" ")
	// 		(a(0), a(1))
	// 	}

	// val testSet = Source.fromFile("data/corpusJSON/dataSplits_trainDevTest/test/all.txt").getLines
	// 	.toList
	// 	.map{ l =>
	// 		val a = l.split(" ")
	// 		(a(0), a(1))
	// 	}


	val dataSets: List[List[(String,String)]] = List(trainSet, devSet, testSet)


	/** takes a list of tuples (e.g. training set tuples) and creates different sublists, 
	not necessarily mutually exclusive ones. By topic, by Perspective_label_frequency_level, and by
	perspective_label_extremity_level
	**/

	def createDataGroups(tuples: List[(String, String)]): List[List[(String, String)]] = {
		
		var para = List[(String, String)]()
		var health = List[(String, String)]()
		var relig = List[(String, String)]()
		var other = List[(String, String)]()
		var higherFreq = List[(String, String)]()
		var lowerFreq = List[(String, String)]()
		var rarFreq = List[(String, String)]()
		var advExt = List[(String, String)]()
		var higherExt = List[(String, String)]()
		var lowerExt = List[(String, String)]()
		//add tupls to the appropriate sets
		tuples.foreach{tupl =>
			val l = tupl._2
			val pl = l//new PerspectiveLabel(l.split("-")(0))
			val r = l//pl.rate
			val v = l//pl.level
			if (l.contains("para")||l.contains("ghost")||l.contains("psychic")||l.matches(".*astro\\W.*")||l.contains("tarot")||l.contains("crypto")||l.contains("aliens")) {para = tupl :: para}
			if (l.contains("relig")) {relig = tupl::relig}
			if (l.contains("health")||l.contains("heatlh")) {health = tupl::health}
			if (!(l.contains("para")||l.contains("ghost")||l.contains("psychic")||l.contains("astro")||l.contains("tarot")||l.contains("crypto")||l.contains("aliens")||l.contains("relig")||l.contains("health")||l.contains("heatlh"))) {other = tupl::other}
			if (r=="mid"|r=="midhi"|r=="hi"|r=="hiext"|r=="ext") {higherFreq = tupl::higherFreq}
			if (!(r=="mid"|r=="midhi"|r=="hi"|r=="hiext"|r=="ext")) {lowerFreq = tupl::lowerFreq}
			if (r=="x"|r=="pf"|r=="rarx"|r=="rar") {rarFreq = tupl::rarFreq}
			if (v=="adv") {advExt = tupl::advExt}
			if (v=="hi"|v=="adv"|v=="hiadv") {higherExt = tupl::higherExt}
			if (!(v=="hi"|v=="adv"|v=="hiadv")) {lowerExt = tupl::lowerExt}
			//if (v=="midhi"|v=="mid"|v=="midlo"|v=="lo"|v=="lobar"|v=="bar") {lowerExt = tupl::lowerExt}
		}
		List(tuples, health, relig, para, other)
	}

	// def createDataGroups(tuples: List[(String, String)]): List[List[(String, String)]] = {
		
	// 	var para = List[(String, String)]()
	// 	var health = List[(String, String)]()
	// 	var relig = List[(String, String)]()
	// 	var other = List[(String, String)]()
	// 	var higherFreq = List[(String, String)]()
	// 	var lowerFreq = List[(String, String)]()
	// 	var rarFreq = List[(String, String)]()
	// 	var advExt = List[(String, String)]()
	// 	var higherExt = List[(String, String)]()
	// 	var lowerExt = List[(String, String)]()
	// 	//add tupls to the appropriate sets
	// 	tuples.foreach{tupl =>
	// 		val l = tupl._2
	// 		val pl = l//new PerspectiveLabel(l.split("-")(0))
	// 		val r = l//pl.rate
	// 		val v = l//pl.level
	// 		if (l.contains("para")||l.contains("ghost")||l.contains("psychic")||l.matches(".*astro\\W.*")||l.contains("tarot")||l.contains("crypto")||l.contains("aliens")) {para = tupl :: para}
	// 		if (l.contains("relig")) {relig = tupl::relig}
	// 		if (l.contains("health")) {health = tupl::health}
	// 		if (!(l.contains("para")||l.contains("ghost")||l.contains("psychic")||l.contains("astro")||l.contains("tarot")||l.contains("crypto")||l.contains("aliens")||l.contains("relig")||l.contains("health")||l.contains("heatlh"))) {other = tupl::other}
	// 		if (r=="mid"|r=="midhi"|r=="hi"|r=="hiext"|r=="ext") {higherFreq = tupl::higherFreq}
	// 		if (!(r=="mid"|r=="midhi"|r=="hi"|r=="hiext"|r=="ext")) {lowerFreq = tupl::lowerFreq}
	// 		if (r=="x"|r=="pf"|r=="rarx"|r=="rar") {rarFreq = tupl::rarFreq}
	// 		if (v=="adv") {advExt = tupl::advExt}
	// 		if (v=="hi"|v=="adv"|v=="hiadv") {higherExt = tupl::higherExt}
	// 		if (!(v=="hi"|v=="adv"|v=="hiadv")) {lowerExt = tupl::lowerExt}
	// 		if (v=="midhi"|v=="mid"|v=="midlo"|v=="lo"|v=="lobar"|v=="bar") {lowerExt = tupl::lowerExt}
	// 	}
	// 	List(tuples, health, relig, para, other, higherFreq, lowerFreq, rarFreq, advExt, higherExt, lowerExt)
	// }




	val trainFileDir= new File(dataSplitsDirPath + dataSetID + "/train/")
	val devFileDir= new File(dataSplitsDirPath + dataSetID + "/dev/")
	val testFileDir= new File(dataSplitsDirPath + dataSetID + "/test/")

	if (!trainFileDir.exists) trainFileDir.mkdir
	if (!devFileDir.exists) devFileDir.mkdir
	if (!testFileDir.exists) testFileDir.mkdir


	val pathEnds = List("all.txt", "health.txt", "relig.txt", "para.txt", "other.txt")
	//val pathEnds = List("all.txt", "health.txt", "relig.txt", "para.txt", "other.txt", "higherFreq.txt", "lowerFreq.txt", "rarFreq.txt", "advExt.txt", "higherExt.txt", "lowerExt.txt")

	val trainGroupFiles = pathEnds.map(end => new File(trainFileDir.getPath + "/" + end))
	val devGroupFiles = pathEnds.map(end => new File(devFileDir.getPath + "/" + end))
	val testGroupFiles = pathEnds.map(end => new File(testFileDir.getPath + "/" + end))

	//write the groups to their own files
	val trainGroupFileTuples: List[(List[(String,String)], File)] = createDataGroups(trainSet).zip(trainGroupFiles)

	trainGroupFileTuples.foreach{ listFileTuple =>
		val tupleList = listFileTuple._1
		val file = listFileTuple._2
		printToFile(file)(p => {
			tupleList.foreach(tupl => p.println(tupl._1 + " " + tupl._2))
			})
	}
	
	/////////////
	val devGroupFileTuples = createDataGroups(devSet).zip(devGroupFiles)

	devGroupFileTuples.foreach{ listFileTuple =>
		val tupleList = listFileTuple._1
		val file = listFileTuple._2
		printToFile(file)(p => {
			tupleList.foreach(tupl => p.println(tupl._1 + " " + tupl._2))
			})
	}
	
	/////////////
	val testGroupFileTuples = createDataGroups(testSet).zip(testGroupFiles)

	testGroupFileTuples.foreach{ listFileTuple =>
		val tupleList = listFileTuple._1
		val file = listFileTuple._2
		printToFile(file)(p => {
			tupleList.foreach(tupl => p.println(tupl._1 + " " + tupl._2))
			})
	}



}//object

object DocumentOrganizer {

	def main(args: Array[String]):Unit = {

		new DocumentOrganizer(args(0))

	}//main
}












class Clean(
	//lastTwoDirs: String = "mainList/main"
	) {

	val corpusJsonPath = "data/corpusJSON/"
	//val corpusDir = new File(corpusJsonPath + lastTwoDirs)
	val mainDir = new File(corpusJsonPath + "mainList/main")
	val oldDir = new File(corpusJsonPath + "oldList/main")

	val newDirForCleanFiles = (new File("data/clean"))


	newDirForCleanFiles.mkdir

	val scDir = new File(newDirForCleanFiles.getPath + "/" + "sc")

	val psDir = new File(newDirForCleanFiles.getPath + "/" + "ps")

	val bothDir = new File(newDirForCleanFiles.getPath + "/" + "combined")

	List(scDir,psDir,bothDir).foreach(dir => dir.mkdir)


	val mainFiles: Array[File] = mainDir.listFiles.filter(_.isFile).filterNot(_.isHidden)
	//println("works after mainfiles")

	val oldFiles: Array[File] =oldDir.listFiles.filter(_.isFile).filterNot(_.isHidden)
	//println("works after oldfiles")

	val files = mainFiles ++ oldFiles
	//val listOfListsOfFiles = List(mainDir, oldDir).map(dir => dir.listFiles.toList.filter(_.isFile).filterNot(_.isHidden))

	//val files = listOfListsOfFiles.flatten






	def makeCleanFiles (f: File): Unit = {

		val lines = Source.fromFile(f).getLines
		val label = lines.next
		val rawJsonLines = lines.toList
		val cleanLines = rawJsonLines.map{ rawJsonLine =>
						//    kpHash,kpPun,kpEmo,kpMent
			getTextNoStopwords(rawJsonLine,false,false,false,false)
		}


		val newFile = {
			if (label.startsWith("sc")) {
					new File(scDir.getPath + "/" + f.getName)
				} else {
					new File(psDir.getPath + "/" + f.getName)
				}
		}


		printToFile(newFile)(p => {
				cleanLines.foreach(p.println)
				})


		val otherNewFile = new File(bothDir.getPath + "/" + f.getName)

		printToFile(otherNewFile)(p => {
				cleanLines.foreach(p.println)
				})
		
	}







	def cleanCorpus (): Unit  = {
		files.foreach(makeCleanFiles)
	}







}//object

object Clean {

	def main(args: Array[String]):Unit = {
	
		new Clean().cleanCorpus

	}//main
}





































class Clean2(
	//lastTwoDirs: String = "mainList/main"
	) {

	val corpusJsonPath = "data/corpusJSON/"
	//val corpusDir = new File(corpusJsonPath + lastTwoDirs)
	val mainDir = new File(corpusJsonPath + "mainList/main")
	val oldDir = new File(corpusJsonPath + "oldList/main")

	val newDirForCleanFiles = (new File("data/clean2"))


	newDirForCleanFiles.mkdir

	val scDir = new File(newDirForCleanFiles.getPath + "/" + "sc")

	val psDir = new File(newDirForCleanFiles.getPath + "/" + "ps")

	val bothDir = new File(newDirForCleanFiles.getPath + "/" + "combined")

	List(scDir,psDir,bothDir).foreach(dir => dir.mkdir)


	val mainFiles: Array[File] = mainDir.listFiles.filter(_.isFile).filterNot(_.isHidden)
	//println("works after mainfiles")

	val oldFiles: Array[File] =oldDir.listFiles.filter(_.isFile).filterNot(_.isHidden)
	//println("works after oldfiles")

	val files = mainFiles ++ oldFiles
	//val listOfListsOfFiles = List(mainDir, oldDir).map(dir => dir.listFiles.toList.filter(_.isFile).filterNot(_.isHidden))

	//val files = listOfListsOfFiles.flatten






	def makeCleanFiles (f: File): Unit = {

		val lines = Source.fromFile(f).getLines
		val label = lines.next
		val rawJsonLines = lines.toList
		val cleanLines = rawJsonLines.map{ rawJsonLine =>
						//    kpHash,kpPun,kpEmo,kpMent
			getTextKeepAll(rawJsonLine)
		}


		val newFile = {
			if (label.startsWith("sc")) {
					new File(scDir.getPath + "/" + f.getName)
				} else {
					new File(psDir.getPath + "/" + f.getName)
				}
		}


		printToFile(newFile)(p => {
				p.println(label)
				cleanLines.foreach(p.println)
				})


		val otherNewFile = new File(bothDir.getPath + "/" + f.getName)

		printToFile(otherNewFile)(p => {
				p.println(label)
				cleanLines.foreach(p.println)
				})
		
	}







	def cleanCorpus (): Unit  = {
		files.foreach(makeCleanFiles)
	}







}//object

object Clean2 {

	def main(args: Array[String]):Unit = {
	
		new Clean2().cleanCorpus

	}//main
}






















































class Clean3(
	//lastTwoDirs: String = "mainList/main"
	) {

	val corpusJsonPath = "data/corpusJSON/"
	//val corpusDir = new File(corpusJsonPath + lastTwoDirs)
	val mainDir = new File(corpusJsonPath + "mainList/confl")
	val oldDir = new File(corpusJsonPath + "oldList/confl")

	val newDirForCleanFiles = (new File("data/clean3"))


	newDirForCleanFiles.mkdir

	val scDir = new File(newDirForCleanFiles.getPath + "/" + "sc")

	val psDir = new File(newDirForCleanFiles.getPath + "/" + "ps")

	val bothDir = new File(newDirForCleanFiles.getPath + "/" + "combined")

	List(scDir,psDir,bothDir).foreach(dir => dir.mkdir)


	val mainFiles: Array[File] = mainDir.listFiles.filter(_.isFile).filterNot(_.isHidden)
	//println("works after mainfiles")

	val oldFiles: Array[File] =oldDir.listFiles.filter(_.isFile).filterNot(_.isHidden)
	//println("works after oldfiles")

	val files = mainFiles ++ oldFiles
	//val listOfListsOfFiles = List(mainDir, oldDir).map(dir => dir.listFiles.toList.filter(_.isFile).filterNot(_.isHidden))

	//val files = listOfListsOfFiles.flatten






	def makeCleanFiles (f: File): Unit = {

		val lines = Source.fromFile(f).getLines
		val label = lines.next
		val rawJsonLines = lines.toList
		val cleanLines = rawJsonLines.map{ rawJsonLine =>
						//    kpHash,kpPun,kpEmo,kpMent
			getTextKeepAll(rawJsonLine)
		}


		val newFile = {
			if (label.startsWith("sc")) {
					new File(scDir.getPath + "/" + f.getName)
				} else {
					new File(psDir.getPath + "/" + f.getName)
				}
		}


		printToFile(newFile)(p => {
				p.println(label)
				cleanLines.foreach(p.println)
				})


		val otherNewFile = new File(bothDir.getPath + "/" + f.getName)

		printToFile(otherNewFile)(p => {
				p.println(label)
				cleanLines.foreach(p.println)
				})
		
	}







	def cleanCorpus (): Unit  = {
		files.foreach(makeCleanFiles)
	}







}//object

object Clean3 {

	def main(args: Array[String]):Unit = {
	
		new Clean3().cleanCorpus

	}//main
}


































// takes the last two dirs of a path to doc collection, prints information about the docs
class Stats (lastTwoDirs: String = "mainList/main", perspective: String) {//perspective should be "sc" or "ps"

	val dirPath = "data/corpusJSON/" + lastTwoDirs
	val dir = new File(dirPath)
	// val files = dir.listFiles.toVector

	// val tuples = files.map{ f => (f.getName.dropRight(4), Source.fromFile(f).getLines.next)

	// 	//create empty lists divided by both topic and perspective
	// var scPara = List[(String, String)]()
	// var psPara = List[(String, String)]()
	// var scHealth = List[(String, String)]()
	// var psHealth = List[(String, String)]()
	// var scRelig = List[(String, String)]()
	// var psRelig = List[(String, String)]()
	// var scOther = List[(String, String)]()
	// var psOther = List[(String, String)]()

	// //add files (as tuples) to appropriate lists
	// fileTuples.foreach{tupl =>
	// 	var inPara = false
	// 	var inPara = false
	// 	var inPara = false
	// 	val l = tupl._2
	// 	if (l.contains("para")||l.contains("ghost")||l.contains("psychic")||l.matches(".*astro\\W.*")||l.contains("tarot")) {
	// 		inPara = true
	// 		if (l.startsWith("sc")) {scPara = tupl::scPara} else {psPara = tupl::psPara}
	// 	}

	// 	if (l.contains("relig")) {
	// 		inRelig = true
	// 		if (l.startsWith("sc")) {scRelig = tupl::scRelig} else {psRelig = tupl::psRelig}
	// 	}

	// 	if (l.contains("health")) {
	// 		inHealth = true
	// 		if (l.startsWith("sc")) {scHealth = tupl::scHealth} else {psHealth = tupl::psHealth}
	// 	}

	// 	if (!(inPara||inRelig||inHealth)) {
	// 		if (l.startsWith("sc")) {scOther = tupl::scOther} else {psOther = tupl::psOther}
	// 	}


	// }


	def printStats(): Unit = {

		//list of files
		val filteredFileList = dir.listFiles.filter(_.isFile).filterNot(_.isHidden)
			.toList
			.filter{f=>
				val label = Source.fromFile(f).getLines.next
				label.startsWith(perspective)
			}

		// list of files => list of tweets in file
		val listOfNumTweetsTuples: List[(Int, Int, String, String)] = filteredFileList.map{ f =>
				val name = f.getName.dropRight(4)
				val it = Source.fromFile(f).getLines
				val label = it.next
				val l = it.toList
				val numTweets = l.length
				val nonRTs = l.filterNot{ tweet =>
					tweet.contains("\"retweeted_status\":")||tweet.split("\\s+").contains("rt")||tweet.split("\\s+").contains("RT")
				}
				val numNonRTs = nonRTs.length
				(numTweets, numNonRTs, name, label)
			}




		val totalDocs = filteredFileList.length
		println("num docs = " + totalDocs)

		val totalTweets = listOfNumTweetsTuples.map(x=>x._1).sum
		println("total tweets = " + totalTweets)

		val totalNonRTs = listOfNumTweetsTuples.map(x=>x._2).sum
		println("total orig tweets = " + totalNonRTs)

		val tweetsPerDoc = totalTweets.toDouble/totalDocs
		println("tweetsPerDoc = " + tweetsPerDoc)

		val origTweetsPerDoc = totalNonRTs.toDouble/totalDocs
		println("origTweetsPerDoc = " + origTweetsPerDoc)

		val proportionOriginal = totalNonRTs.toDouble/totalTweets
		println("proportion original = " + proportionOriginal)


		val minTweets = listOfNumTweetsTuples.map(x=>x._1).min
		println("min # of Tweets = " + minTweets)
		val maxTweets = listOfNumTweetsTuples.map(x=>x._1).max
		println("max # of Tweets = " + maxTweets)


	}

}



object Stats {

	def main(args: Array[String]):Unit = {

		new Stats(args(0), args(1)).printStats()

	}//main
}









class AllStats () {//perspective should be "sc" or "ps"

	// val mainDirPath = "data/corpusJSON/mainList/main"
	// val oldDirPath = "data/corpusJSON/oldList/main"
	// val mainDir = new File(mainDirPath)
	// val oldDir = new File(oldDirPath)
	// val files = mainDir.listFiles.toVector ++ oldDir.listFiles.toVector
	val pathsOfPaths = List(
		// "data/corpusJSON/dataSplitsDir/full/train/all.txt"
		// "data/corpusJSON/dataSplitsDir/full/dev/all.txt"
		"data/corpusJSON/dataSplitsDir/full/test/all.txt"
		)

	val files = pathsOfPaths.map(path => Source.fromFile(path).getLines.toList).flatten.map(line => line.split("\\s+")(0)).map(filename => new File(filename))

	println("number of files: " + files.length)

	

	def printStats(): Unit = {

		//list of files
		val filteredFileList = files.filter(_.isFile).filterNot(_.isHidden)
			.toList


		println("diff")
		(files.toSet -- filteredFileList.toSet).foreach(println)

		// list of files => list of tweets in file
		val listOfNumTweetsTuples: List[(Int, Int, String, String, Int)] = filteredFileList.map{ f =>
				val name = f.getName.dropRight(4)
				val it = Source.fromFile(f).getLines
				val label = it.next
				val l = it.toList
				val numTweets = l.length
				val nonRTs = l.filterNot{ tweet =>
					tweet.contains("\"retweeted_status\":")||tweet.split("\\s+").contains("rt")||tweet.split("\\s+").contains("RT")
				}
				val numNonRTs = nonRTs.length
				val wordCount = l.map(x=>getText(x,true,false,false,false)).flatMap(x=>x.split("\\s+").toList).length
				(numTweets, numNonRTs, name, label, wordCount)
			}


		val totalDocs = filteredFileList.length
		println("num docs = " + totalDocs)

		val totalWords = listOfNumTweetsTuples.map(x=>x._5).sum
		println("total words = " + totalWords)

		val totalTweets = listOfNumTweetsTuples.map(x=>x._1).sum
		println("total tweets = " + totalTweets)

		val totalNonRTs = listOfNumTweetsTuples.map(x=>x._2).sum
		println("total orig tweets = " + totalNonRTs)

		val tweetsPerDoc = totalTweets.toDouble/totalDocs
		println("tweetsPerDoc = " + tweetsPerDoc)

		val origTweetsPerDoc = totalNonRTs.toDouble/totalDocs
		println("origTweetsPerDoc = " + origTweetsPerDoc)

		val proportionOriginal = totalNonRTs.toDouble/totalTweets
		println("proportion original = " + proportionOriginal)


		val minTweets = listOfNumTweetsTuples.map(x=>x._1).min
		println("min # of Tweets = " + minTweets)
		val maxTweets = listOfNumTweetsTuples.map(x=>x._1).max
		println("max # of Tweets = " + maxTweets)

		// listOfNumTweetsTuples.filter(x=>x._1 < 1000).foreach(println)


		// listOfNumTweetsTuples.foreach(x=> x match {
		// 	case x if x._1 < 200 => println("200")
		// 	case x if x._1 < 300 => println("300")
		// 	case x if x._1 < 400 => println("400")
		// 	case x if x._1 < 500 => println("500")
		// 	case x if x._1 < 1000 => println("1000")
		// 	case _ => println("+thousand")
		// 	})

	}

def printStats(persp: String): Unit = {

		//list of files
		val filteredFileList0 = files.filter(_.isFile).filterNot(_.isHidden)
			.toList

		val filteredFileList = filteredFileList0.filter(x => Source.fromFile(x).getLines.next.startsWith(persp))


		// list of files => list of tweets in file
		val listOfNumTweetsTuples: List[(Int, Int, String, String, Int)] = filteredFileList.map{ f =>
				val name = f.getName.dropRight(4)
				val it = Source.fromFile(f).getLines
				val label = it.next
				val l = it.toList
				val numTweets = l.length
				val nonRTs = l.filterNot{ tweet =>
					tweet.contains("\"retweeted_status\":")||tweet.split("\\s+").contains("rt")||tweet.split("\\s+").contains("RT")
				}
				val numNonRTs = nonRTs.length
				val wordCount = l.map(x=>getText(x,true,false,false,false)).flatMap(x=>x.split("\\s+").toList).length
				(numTweets, numNonRTs, name, label, wordCount)
			}


		val totalDocs = filteredFileList.length
		println("num docs = " + totalDocs)

		val totalWords = listOfNumTweetsTuples.map(x=>x._5).sum
		println("total words = " + totalWords)

		val totalTweets = listOfNumTweetsTuples.map(x=>x._1).sum
		println("total tweets = " + totalTweets)

		val totalNonRTs = listOfNumTweetsTuples.map(x=>x._2).sum
		println("total orig tweets = " + totalNonRTs)

		val tweetsPerDoc = totalTweets.toDouble/totalDocs
		println("tweetsPerDoc = " + tweetsPerDoc)

		val origTweetsPerDoc = totalNonRTs.toDouble/totalDocs
		println("origTweetsPerDoc = " + origTweetsPerDoc)

		val proportionOriginal = totalNonRTs.toDouble/totalTweets
		println("proportion original = " + proportionOriginal)


		val minTweets = listOfNumTweetsTuples.map(x=>x._1).min
		println("min # of Tweets = " + minTweets)
		val maxTweets = listOfNumTweetsTuples.map(x=>x._1).max
		println("max # of Tweets = " + maxTweets)

		listOfNumTweetsTuples.filter(x=>x._1 < 100).foreach(println)


		// listOfNumTweetsTuples.foreach(x=> x match {
		// 	case x if x._1 < 200 => println("200")
		// 	case x if x._1 < 300 => println("300")
		// 	case x if x._1 < 400 => println("400")
		// 	case x if x._1 < 500 => println("500")
		// 	case x if x._1 < 1000 => println("1000")
		// 	case _ => println("+thousand")
		// 	})

	}



}



object AllStats {

	def main(args: Array[String]):Unit = {

		val x = new AllStats()
		x.printStats()
		println("PRO-SCIENCE: ")
		x.printStats("sc")
		println("")
		println("non-pro-science: ")
		x.printStats("ps")

	}//main
}




































// object CleanFiles {

// 	import thesiscode.util.Util._

// 	val mainCorpusDir = new File("data/corpusJSON/mainList/main")
// 	val oldCorpusDir = new File("data/corpusJSON/oldList/main")
// 	val mainFiles: Array[File] = mainCorpusDir.listFiles.filter(_.isFile).filterNot(_.isHidden).toVector   //.slice(0,1)
// 	val oldFiles: Array[File] = oldCorpusDir.listFiles.filter(_.isFile).filterNot(_.isHidden).toVector
// 	val files = mainFiles ++ oldFiles 
// 	//println(files.length)
// 	val newPath = "data/corpus/clean/"



// 	def makeCleanVersionOfFile (f: File): Unit = {
// 		val rawLines = Source.fromFile(f).getLines
// 		val textLines = rawLines.map(x=> getText(x, false, false, false, false))


// 		// val filtered1 = ridEmoji(basicText) //no Emoji
// 		// val filtered2 = basicText.split("\\s+") // no hashtags/mentions
// 		// 	.filterNot(_.startsWith("@"))
// 		// 	.filterNot(_.startsWith("#"))
// 		// 	.filterNot(x => x.contains("bit.ly")||x.contains("t.co")||x.contains("http")||x.contains(".com")||x.contains("ow.ly")||x.contains(".net")||x.contains("www.")||x.contains("tinyUrl"))
// 		// 	.mkString(" ")
// 		// val tokenized = nak.util.CleanStringTokenizer(filtered1)
// 		// val cleanText = tokenized.mkString(" ")

// 		// val newFile = new File(newPath + f.getName)

// 		// printToFile(newFile)(p => {
// 		// 	p.println(cleanText)
// 		// 	})

// 	}


// 	def main (args: Array[String]): Unit = {

// 		if (new File(newPath).listFiles.filterNot(_.isHidden).isEmpty) {
// 			files.foreach(makeCleanFiles)
// 		}


// 	}

// }


// object Old {



// 	val linesMain = Source.fromFile("data/twitterUsers/mainList.txt").getLines
// 	val filteredMain = linesMain.filterNot(_.startsWith("\"")).toVector
// 	val numMain = filteredMain.length
// 	println("num main: " + numMain)
// 	val mainNames = filteredMain.map(line=>line.split("\\s+")(0)).toSet

// 	val linesOld = Source.fromFile("data/twitterUsers/oldList.txt").getLines
// 	val filteredOld = linesOld.filterNot(_.startsWith("\"")).toVector
// 	val numOld = filteredOld.length
// 	println("num old: " + numOld)
// 	val noDupes = filteredOld.filterNot{line =>
// 		val name = line.split("\\s+")(0)
// 		mainNames(name)
// 	}

// 	def main (args: Array[String]) {
// 		println("num exclusively in old: " + noDupes.length + "\n")
// 		noDupes.foreach(println)

// 	}




// }



// object CleanJsonFiles {

// 	import thesiscode.util.Util._

// 	val corpusDir = new File("data/corpusJSON/byUser")
// 	val files: Array[File] = corpusDir.listFiles.filter(_.isFile).filterNot(_.isHidden)//.slice(0,1)
// 	//println(files.length)
// 	val newPath = "data/corpusJSON/clean/"

// 	def makeCleanFiles (f: File, both: boolean = true): Unit = {
// 		val lines = Source.fromFile(f).getLines
// 		lines.next
// 		val textLines1 = lines.map(json => getText(json))


// 		//textLines1.foreach(println)
// 		// val textLines2 = textLines1.
// 		// val filtered1 = ridEmoji(basicText) //no Emoji
// 		// val filtered2 = basicText.split("\\s+") // no hashtags/mentions
// 		// 	.filterNot(_.startsWith("@"))
// 		// 	.filterNot(_.startsWith("#"))
// 		// 	.filterNot(x => x.contains("bit.ly")||x.contains("t.co")||x.contains("http")||x.contains(".com")||x.contains("ow.ly")||x.contains(".net")||x.contains("www.")||x.contains("tinyUrl"))
// 		// 	.mkString(" ")
// 		// val tokenized = nak.util.CleanStringTokenizer(filtered1)
// 		// val cleanText = tokenized.mkString(" ")

// 		// val newFile = new File(newPath + f.getName)

// 		// printToFile(newFile)(p => {
// 		// 	p.println(cleanText)
// 		// 	})

// 	}


// 	def main (args: Array[String]): Unit = {

// 		if (new File(newPath).listFiles.filterNot(_.isHidden).isEmpty) {
// 			files.foreach(makeCleanFiles)
// 		}


// 	}

// }



object RunTest {
	def main (args: List[String]) = {
		println(args(0))
	}
}


