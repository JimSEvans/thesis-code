package thesiscode.dataprocessing

import scala.collection.mutable
import scala.io.Source
import java.io.File
import thesiscode.util.Util._
import scala.math.BigDecimal


class RelativeFrequencyRatios(
	// lastTwoDirs: String = "mainList/main"
	) {

	// val corpusJsonPath = "data/corpusJSON/"
	// val corpusDir = new File(corpusJsonPath + lastTwoDirs)
	val mainDir = new File("data/corpusJSON/mainList/main")
	val oldDir = new File("data/corpusJSON/oldList/main")
	val files: Vector[File] = (mainDir.listFiles.toVector ++ oldDir.listFiles.toVector).filter(_.isFile).filterNot(_.isHidden)

	var scFiles = List[File]()
	var psFiles = List[File]()
	var neutralFiles = List[File](new File("data/corpusJSON/spritzer.txt"))

	files.foreach{f =>
		val l = Source.fromFile(f).getLines.next
		if (l.startsWith("sc")) {
			scFiles = f::scFiles
		}
		else {psFiles = f::psFiles}
	}

	val wordCountMap_sc = mutable.Map[String,Int]().withDefault(x=>0)
	val wordCountMap_ps = mutable.Map[String,Int]().withDefault(x=>0)
	val wordCountMap_neutral = mutable.Map[String,Int]().withDefault(x=>0)



	def getCountsForJsonLine(m: mutable.Map[String,Int], line: String) = {
		val text = getTextNoStopwords(line,false,false,false,false)
		val text2 = ridPunctRC(text)
		val cleanArray = text.split("\\s+")
		cleanArray.foreach{ token =>
			m(token) += 1
		}
	}

	def getCountsForFile(m: mutable.Map[String,Int], f: File) = {
		val src = Source.fromFile(f)
		val lines = src.getLines
		lines.next // get rid of first line, which is the doc label
		lines.foreach(line => getCountsForJsonLine(m, line))
		src.close()
	}


	scFiles.foreach(f => getCountsForFile(wordCountMap_sc,f))

	psFiles.foreach(f => getCountsForFile(wordCountMap_ps,f))

	neutralFiles.foreach(f => getCountsForFile(wordCountMap_neutral,f))



	val scKeys = wordCountMap_sc.keys
	val psKeys = wordCountMap_ps.keys
	val neutralKeys = wordCountMap_neutral.keys

	val scUniq = scKeys.toVector.filterNot(psKeys.toSet++neutralKeys.toSet)
	val psUniq = psKeys.toVector.filterNot(scKeys.toSet++neutralKeys.toSet)
	val neutralUniq = psKeys.toVector.filterNot(scKeys.toSet++psKeys.toSet)

	scUniq.foreach{ token =>
		wordCountMap_ps(token) += 1
		wordCountMap_neutral(token) += 1
	}

	psUniq.foreach{ token =>
		wordCountMap_sc(token) += 1
		wordCountMap_neutral(token) += 1
	}

	neutralUniq.foreach{ token =>
		wordCountMap_sc(token) += 1
		wordCountMap_ps(token) += 1
	}

	var scWordTotal = 0
	wordCountMap_sc.values.foreach(x => scWordTotal += x)
	var psWordTotal = 0
	wordCountMap_ps.values.foreach(x => psWordTotal += x)
	var neutralWordTotal = 0
	wordCountMap_neutral.values.foreach(x => neutralWordTotal += x)

	val keys = wordCountMap_neutral.keys

	val relativeFrequencyRatios = keys.map{ key =>
		val scCount = wordCountMap_sc(key)
		val psCount = wordCountMap_ps(key)
		val neutralCount = wordCountMap_neutral(key)
		val sc =  scCount.toDouble/scWordTotal
		val ps = psCount.toDouble/psWordTotal
		val neutral = neutralCount.toDouble/neutralWordTotal
		val scRatio = sc/neutral
		val psRatio = ps/neutral

		val diff = (scRatio - psRatio).abs
		(key, diff, scCount, psCount, neutralCount, scRatio, psRatio)
	}


	val sorted0 = relativeFrequencyRatios.toList.sortBy(x=>x._2).reverse

	val sorted = sorted0.filter(tuple => tuple._3 >= 2 && tuple._4 >= 2 && tuple._5 >= 2)
	// val sorted = "key,diff,scCount,psCount,neutralCount,scRatio,psRatio" :: sorte

	// val freqMap_sc = wordCountMap_sc.mapValues(count => count/scWordTotal.toDouble)
	// val freqMap_ps = wordCountMap_ps.mapValues(count => count/psWordTotal.toDouble)

	// val scOrder = freqMap_sc.toList.sorted
	// val psOrder = freqMap_ps.toList.sorted
	// val zipped0: List[((String, Double),(String, Double))] = scOrder.zip(psOrder)
	// val zipped = zipped0.sortBy(x=>(x._1._2 - x._2._2).abs).reverse

		// println("hello")

	// val toPrint = zipped.map{tuplOftuples =>
	// 	val scTupl = tuplOftuples._1
	// 	val psTupl = tuplOftuples._2
	// 	(scTupl._1 + " " + scTupl._2.toString + "\t" + psTupl._2.toString)
	//}

	//val Array(listDirName, typeDirName) = lastTwoDirs.split("/") // cf. Array(listDirName, typeDirName) = Array("mainList","main")
		
	val writeToFile = new File("data/freqs.txt")

	// if (!writeToDir.exists) writeToDir.mkdir

	// val writeToFile = new File(writeToDir.getPath + "/" + typeDirName + ".csv")

	printToFile(writeToFile)(p => {
		p.println("word & diff & psCount & nsCount & neutralCount & psRatio & nsRatio")
		sorted.foreach{ tupl => 
			val (word0, diff0, scCount, psCount, neutralCount, scRatio0, psRatio0) = tupl
			val word = if (scRatio0 < psRatio0) word0 else {"""\emph{""" + word0 + """}"""}
			val diff = BigDecimal(diff0.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
			val scRatio = BigDecimal(scRatio0.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
			val psRatio = BigDecimal(psRatio0.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
				p.println(
					word + " & " + diff + " & " + scCount + " & " + psCount + " & " + neutralCount + " & " + scRatio + " & " + psRatio
					)
			}
		})

	// printToFile(writeToFile)(p => {
	// 	p.println("key,diff,scCount,psCount,neutralCount,scRatio,psRatio")
	// 	sorted.foreach(tupl => p.println(tupl._1 + "," + tupl._2 + "," + tupl._3 + "," + tupl._4 + "," + tupl._5 + "," + tupl._6 + "," + tupl._7))
	// 	})

 }



object RelativeFrequencyRatios {


	def main (args: Array[String]): Unit = {

		new RelativeFrequencyRatios()

	}

}


















class LabelDistribution (
	listOfFilesFiles: List[String] = List(
		// "data/corpusJSON/dataSplitsDir/full/train/all.txt",
		"data/corpusJSON/dataSplitsDir/full/dev/all.txt"
		// "data/corpusJSON/dataSplitsDir/full/test/all.txt"
		)
	) {

	println("\"list of files\" files: ")
	listOfFilesFiles.foreach(println)

	val allFileNamesLabelTuples
		 = listOfFilesFiles
			.toVector
			.map{string => Source.fromFile(string).getLines.toVector}
			.flatten
			.map{ string =>
				val split = string.split("\\s+")
				(split(0), split(1))
			}

	println("number of files: " + allFileNamesLabelTuples.length)

		//create empty lists divided by both topic and perspective
	var scPara = List[(String, String)]()
	var psPara = List[(String, String)]()
	var scHealth = List[(String, String)]()
	var psHealth = List[(String, String)]()
	var scRelig = List[(String, String)]()
	var psRelig = List[(String, String)]()
	var scOther = List[(String, String)]()
	var psOther = List[(String, String)]()

	//add files (as tuples) to appropriate lists
	allFileNamesLabelTuples.foreach{ tupl =>
		var inPara = false
		var inRelig = false
		var inHealth = false
		val l = tupl._2
		if (l.contains("para")||l.contains("ghost")||l.contains("psychic")||l.matches(".*astro\\W.*")||l.contains("tarot")||l.contains("crypto")||l.contains("aliens")) {
			inPara = true
			if (l.startsWith("sc")) {scPara = tupl::scPara} else {psPara = tupl::psPara}
		}

		if (l.contains("relig")) {
			inRelig = true
			if (l.startsWith("sc")) {scRelig = tupl::scRelig} else {psRelig = tupl::psRelig}
		}

		if (l.contains("health")) {
			inHealth = true
			if (l.startsWith("sc")) {scHealth = tupl::scHealth} else {psHealth = tupl::psHealth}
		}

		if (!(inPara||inRelig||inHealth)) {
			if (l.startsWith("sc")) {scOther = tupl::scOther} else {psOther = tupl::psOther}
		}
	}

	var sc = List[(String, String)]()
	var ps = List[(String, String)]()




	allFileNamesLabelTuples.foreach{ tupl =>
		val l = tupl._2
		if (l.startsWith("sc")) {sc = tupl::sc}
		else if (l.startsWith("ps")) {ps = tupl::ps}
		else println("NEITHER PERSPECTIVE" + tupl._1)
	}











	def print() {

		println("Health")
		println("sc: " + scHealth.length + "; ps: " + psHealth.length + "; total: " + (scHealth.length + psHealth.length))
		println("")

		println("Relig")
		println("sc: " + scRelig.length + "; ps: " + psRelig.length + "; total: " + (scRelig.length + psRelig.length))
		println("")

		println("Para")
		println("sc: " + scPara.length + "; ps: " + psPara.length + "; total: " + (scPara.length + psPara.length))
		println("")

		println("Other")
		println("sc: " + scOther.length + "; ps: " + psOther.length + "; total: " + (scOther.length + psOther.length))
		println("")

		psOther.foreach(println)
		scOther.foreach(println)

		println("Total")
		val scTotal = scOther.length + scPara.length + scRelig.length + scHealth.length
		val psTotal = psOther.length + psPara.length + psRelig.length + psHealth.length

		println("sc: " + scTotal + "; ps: " + psTotal + "; total: " + (scTotal+psTotal))

	}


	val scTopics = 	Vector(scHealth, scRelig, scPara)
	val psTopics = Vector(psHealth, psRelig, psPara)

	var scx = List[(String, String)]()
	var psx = List[(String, String)]()




	def print2() {

		sc.foreach{ tupl =>
			val name = tupl._1
			val label = tupl._2
			val topics = scTopics.map(topic => topic.contains(tupl)).zip(Vector("h", "r", "p"))
				.filter(x=>x._1==true)
				.map(x=>x._2)
				.toSet.toVector.sorted.mkString("+")
			scx = (name, topics)::scx
		}

		ps.foreach{ tupl =>
			val name = tupl._1
			val label = tupl._2
			val topics = psTopics.map(topic => topic.contains(tupl)).zip(Vector("h", "r", "p"))
				.filter(x=>x._1==true)
				.map(x=>x._2)
				.toSet.toVector.sorted.mkString("+")
			psx = (name, topics)::psx
		}


		var scMap = scx.map(t=>t._2).groupBy(x=>x).mapValues(set=>set.size).withDefault(x=>0)

		// scMap = scMap + ("h+p+r" -> 0)

		var psMap = psx.map(t=>t._2).groupBy(x=>x).mapValues(set=>set.size).withDefault(x=>0)

		val zipped = scMap.zip(psMap)

		val combos = List("h", "h+r", "h+p+r", "r", "p+r", "p")

		var rows: List[List[Int]] = combos.map{ combo =>
			val scNum = scMap(combo)
			val psNum = psMap(combo)
			val totalNum = (scMap(combo) + psMap(combo))
			List(scNum, psNum, totalNum)
			// println(
			// 	combo + " & " scMap(combo) + " & " + psMap(combo) + " & " + total + """ \\
			// 	)
		}

		val totalSc = rows.map(x=>x(0)).sum

		val totalPs = rows.map(x=>x(1)).sum

		val tTotal = rows.map(x=>x(2)).sum


		val totalRow: List[Int] = List(totalSc, totalPs, tTotal)

		rows = rows ++ List(totalRow)

		def getWithPercent(num: Int): String = {
			val prop = num.toDouble/tTotal*100
			val rounded = BigDecimal(prop).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString
			num + " (" + rounded + """\%)"""
		}

		val withPercent = rows.map(row=> row.map(getWithPercent))

		val combos2 = combos ++ List("total")


		val lines = withPercent.zipWithIndex.map{x =>
			val i = x._2
			val combin = combos2(i) match {
				case "h" => "health"
				case "h+r" => "health + relig"
				case "h+p+r" => "health + relig + paranormal"
				case "r" => "relig"
				case "p+r" => "relig + paranormal"
				case "p" => "paranormal"
				case _ => combos2(i)
			}
			combin + " & " + x._1.mkString(" & ") + """ \\"""
		}

		lines.foreach(println)

		println(combos2)


		// println("Science user")

		// scMap.keys.toVector.sorted.foreach(x=> println(x + " " + scMap(x)))

		// println("Non-Science user")

		// psMap.keys.toVector.sorted.foreach(x=> println(x + " " + psMap(x)))

	}

}





object LabelDistribution {
	def main (args: Array[String]): Unit = {

		val set = if (args.length != 0) List(args(0)) else List("none")

		if (set(0).endsWith(".txt")) {

			// new LabelDistribution(set).print()

			new LabelDistribution(set).print2()

		} else {

			// new LabelDistribution().print()

			new LabelDistribution().print2()
		}

	}
}


















object RelativeFrequencyRatios2 {

	import scala.math.BigDecimal

	def main (args: Array[String]) {

	val lines = Source.fromFile("data/freqs/redone.csv").getLines
	val header = lines.next
	// val newFile = new File("data/freqs/redone.csv")

	// val newLines = lines.map{ line =>
	// 	val Array(key,diff,scCount,psCount,neutralCount,scRatio,psRatio) = line.split(",")
	// 	val newKey =
	// 		if (scRatio.toDouble > psRatio.toDouble) {"*" + key} else key
	// 	val newLine = Array(newKey,diff,scCount,psCount,neutralCount,scRatio,psRatio).mkString(",")
	// 	newLine
	// }

	val newArrays = lines.map{ line =>
		val Array(key,diff,scCount,psCount,neutralCount,scRatio,psRatio) = line.split(",")
		val newKey = if (scRatio.toDouble > psRatio.toDouble) {"""\emp{"""+key.drop(1)+"""}"""} else key
		Array(newKey,
			BigDecimal(diff.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble.toString,
			scCount,
			psCount,
			neutralCount,
			BigDecimal(scRatio.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble.toString,
			BigDecimal(psRatio.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble.toString)
	}

	def tex (array: Array[String]) = {

	    array.dropRight(1).foreach{
	    	x => print(x + " & ")
			}
		print(array.last)
		println(""" \\""")
	    // ~ & ~ & ~ & ~ & ~ & ~ & ~ \\

	}



		println("""\begin{table}
    \begin{tabular}{lllllll}""")

	newArrays.foreach(a => tex(a))

	   	println(""" \end{tabular}
	   		\end{table}""")

	// printToFile(newFile)(p => {
	// 		p.println(header)
	// 		newLines.foreach(x=> p.println(x))
	// 	})
	}
}



object ArticlesTable {

	val BlogRE = """([^/]+)(?:/.+|$)""".r

	def labeledArticles ()
			(implicit codec: scala.io.Codec) = { // sliceMethod possibilities = "none", "new", "old", "random"
			// println("ALL TWEETS FUNCTION")
			val dir = new File("data/Articles/data")
		    for {
		    	articleFile <- dir.listFiles.toVector.filter(_.isFile).filterNot(_.isHidden)
		    } yield {

		    	val fileSource = Source.fromFile(articleFile)
		    	val lines = fileSource.getLines
		    	val firstLine = lines.next
		    	val label = if (firstLine.startsWith("ps")) {"ps"} else if (firstLine.startsWith("sc")) {"sc"} else {println(articleFile.getName); "none"}

		    	val rest = lines.mkString("\n")
		    	val split = rest.split("""\n\s*\n\s*""")
		    	val metadata = split(0)
		    	val metadataLines = metadata.split("\n")
		    	val Array(url, title, author) = metadataLines.take(3)
		    	val foo = url
		    		.replaceAllLiterally("http://", "")
		    		.replaceAllLiterally("www.", "")
		    	val BlogRE(blogUrl) = foo
		    	fileSource.close
		    	//println (fileName + " " + fullText.split("\n").length)
		    	(label, blogUrl, author, title)
		    }

		  }


	def main (args: Array[String]) {
		val tuples = labeledArticles()
		val author2tuples = tuples.groupBy(tupl => tupl._3)

		val author2blog2title = author2tuples.mapValues{ arrayOfTuples => 
			arrayOfTuples
				.groupBy(x=>x._2)
				.mapValues{array => 
					array.map(tupl => tupl._4)
				}
		}

		author2blog2title.keys.toList.sorted.foreach { author => 
			println(author)
			val blog2title = author2blog2title(author)
			blog2title.keys.toList.sorted.foreach { blog => 
				println("\t" + blog + " " + blog2title(blog).length)
			}
		}

		println(author2blog2title.keys.toList.length)
		//tuples.foreach(x=>println(x._2 + " " + x._4 + " " + x._5))
		//m.keys.foreach(k => println(k + " " + m(k)))

	}
}







object Blogs {

	val BlogRE = """([^/]+)(?:/.+|$)""".r

	def labeledArticles ()
			(implicit codec: scala.io.Codec) = { // sliceMethod possibilities = "none", "new", "old", "random"
			// println("ALL TWEETS FUNCTION")
			val dir = new File("data/Articles/data")
		    for {
		    	articleFile <- dir.listFiles.toVector.filter(_.isFile).filterNot(_.isHidden)
		    } yield {

		    	val fileSource = Source.fromFile(articleFile)
		    	val lines = fileSource.getLines
		    	val firstLine = lines.next
		    	val label = if (firstLine.startsWith("ps")) {"ps"} else if (firstLine.startsWith("sc")) {"sc"} else {println(articleFile.getName); "none"}

		    	val rest = lines.mkString("\n")
		    	val split = rest.split("""\n\s*\n\s*""")
		    	val metadata = split(0)
		    	val metadataLines = metadata.split("\n")
		    	val Array(url, title, author) = metadataLines.take(3)
		    	val foo = url
		    		.replaceAllLiterally("http://", "")
		    		.replaceAllLiterally("www.", "")
		    	val BlogRE(blogUrl) = foo
		    	fileSource.close
		    	//println (fileName + " " + fullText.split("\n").length)
		    	(label, blogUrl, author, title)
		    }

		  }


	def main (args: Array[String]) {
		val tuples = labeledArticles()
		val blogs2tuples = tuples.groupBy(tupl => tupl._2)
		val blogs2counts = blogs2tuples.mapValues(arr=>arr.length).foreach(tupl => println(tupl._1 + " & " + tupl._2 + """ \\ \hline"""))
	}
}
















object Authors {

	val BlogRE = """([^/]+)(?:/.+|$)""".r

	def labeledArticles ()
			(implicit codec: scala.io.Codec) = { // sliceMethod possibilities = "none", "new", "old", "random"
			// println("ALL TWEETS FUNCTION")
			val dir = new File("data/Articles/data")
		    for {
		    	articleFile <- dir.listFiles.toVector.filter(_.isFile).filterNot(_.isHidden)
		    } yield {

		    	val fileSource = Source.fromFile(articleFile)
		    	val lines = fileSource.getLines
		    	val firstLine = lines.next
		    	val label = if (firstLine.startsWith("ps")) {"ps"} else if (firstLine.startsWith("sc")) {"sc"} else {println(articleFile.getName); "none"}

		    	val rest = lines.mkString("\n")
		    	val split = rest.split("""\n\s*\n\s*""")
		    	val metadata = split(0)
		    	val metadataLines = metadata.split("\n")
		    	val Array(url, title, author) = metadataLines.take(3)
		    	val foo = url
		    		.replaceAllLiterally("http://", "")
		    		.replaceAllLiterally("www.", "")
		    	val BlogRE(blogUrl) = foo
		    	fileSource.close
		    	//println (fileName + " " + fullText.split("\n").length)
		    	(label, blogUrl, author.toLowerCase, title)
		    }

		  }


	def main (args: Array[String]) {
		val tuples = labeledArticles()
		val authors2tuples = tuples.groupBy(tupl => tupl._3)
		val authors2counts = authors2tuples.mapValues(arr=>arr.length)
		val biggestAuthors = authors2counts.toList.sortBy(tupl=>tupl._2).reverse.slice(0,4)
		biggestAuthors.foreach(println)
		// .foreach(tupl => println(tupl._1 + " & " + tupl._2 + """ \\ \hline"""))
	}
}











object ArticleStats {

	val BlogRE = """([^/]+)(?:/.+|$)""".r

	def labeledArticles ()
			(implicit codec: scala.io.Codec) = { // sliceMethod possibilities = "none", "new", "old", "random"
			// println("ALL TWEETS FUNCTION")
			val dir = new File("data/Articles/data")
		    for {
		    	articleFile <- dir.listFiles.toVector.filter(_.isFile).filterNot(_.isHidden)
		    } yield {

		    	val fileSource = Source.fromFile(articleFile)
		    	val lines = fileSource.getLines
		    	val firstLine = lines.next
		    	val label = if (firstLine.startsWith("ps")) {"ps"} else if (firstLine.startsWith("sc")) {"sc"} else {println(articleFile.getName); "none"}

		    	val rest = lines.mkString("\n")
		    	val split = rest.split("""\n\s*\n\s*""")
		    	val metadata = split(0)
		    	val metadataLines = metadata.split("\n")
		    	val Array(url, title, author) = metadataLines.take(3)
		    	val foo = url
		    		.replaceAllLiterally("http://", "")
		    		.replaceAllLiterally("www.", "")
		    	val BlogRE(blogUrl) = foo

		    	val text = split.drop(1).mkString("\n")
		    	val wordCount = text.split("\\s+").length

		    	fileSource.close
		    	//println (fileName + " " + fullText.split("\n").length)
		    	(label, blogUrl, author, title, wordCount)
		    }

		  }


	def main (args: Array[String]) {
		val tuples = labeledArticles()
		val sc = tuples.filter(tupl => tupl._1 == "sc")
		val ps = tuples.filter(tupl => tupl._1 == "ps")

		val totalWords = tuples.map(x=>x._5).sum
		val scWords = sc.map(x=>x._5).sum
		val psWords = ps.map(x=>x._5).sum

		val totalDocs = tuples.length
		val scDocs = sc.length
		val psDocs = ps.length

		println("scDocs :" + scDocs)
		println("psDocs :" + psDocs)
		println("totalDocs :" + totalDocs)
		println("scwords :" + scWords)
		println("pswords :" + psWords)
		println("totalwords :" + totalWords)


		// val author2tuples = tuples.groupBy(tupl => tupl._3)

		// val author2blog2title = author2tuples.mapValues{ arrayOfTuples => 
		// 	arrayOfTuples
		// 		.groupBy(x=>x._2)
		// 		.mapValues{array => 
		// 			array.map(tupl => tupl._4)
		// 		}
		// }

		// author2blog2title.keys.toList.sorted.foreach { author => 
		// 	println(author)
		// 	val blog2title = author2blog2title(author)
		// 	blog2title.keys.toList.sorted.foreach { blog => 
		// 		println("\t" + blog + " " + blog2title(blog).length)
		// 	}
		// }

		// println(author2blog2title.keys.toList.length)
		// //tuples.foreach(x=>println(x._2 + " " + x._4 + " " + x._5))
		// //m.keys.foreach(k => println(k + " " + m(k)))

	}
}







// object O {

// 	val table = """ h     & 372 (28.2\%) & 112 (8.5\%)    & 484 (36.7\%)  \\
// 			    h + r     & 16 (1.2\%) & 21(1.6\%)      & 37 (2.8\%)   \\
// 			    h + p     & 5 (0.4\%)  & 3 (0.2\%)      & 8  (0.6\%) \\
// 			    h + r + p & 0 (0.0\%)    & 5 (0.4\%)      & 5  (0.4\%)   \\
// 			    r         & 223 (16.9\%) & 334 (25.3\%)   & 557 (42.3\%) \\
// 			    r + p     & 8 (0.6\%)    & 39 (29.6\%)    & 47   (3.6\%) \\
// 			    p         & 26 (2.0\%)   & 154 (11.7\%)   & 180   (13.7\%) \\
// 			    total     & 650 (49.3\%) & 668 (50.7\%)   & 1318 (100.0\%) \\"""

//     val lines = table.split("\n")

//     val RE = .+

//     val splitLines = lines.map{line=>
//     	val split = line.split(""" *& *""")).drop(1)


// }


