package thesiscode.dataprocessing

import thesiscode.util.Util._
import java.io.{File,FileWriter,PrintWriter}
import scala.sys.process._
import scala.util.Random.shuffle
import scala.io.Source
import scala.math.BigDecimal

object Tex {

	val results = """(0, 19) -> 5350199867861301
(1, 24) -> 3881805714967492
(2, 28) -> 5602459639353328
(3, 22) -> 5537007399803957
(4, 23) -> 2757764119137043
(5, 13) -> 4794419838412900
(6, 7) -> 4992014802156626
(7, 6) -> 5996539731578048
(8, 8) -> 2726472692704895
(9, 10) -> 6203755160058834
(10, 11) -> 3470719394071889
(11, 16) -> 3702948014340693
(12, 12) -> 5112228078480052
(13, 9) -> 3787434801977976
(14, 18) -> 5145103308644003
(15, 4) -> 6402054562156804
(16, 2) -> 3527234303442950
(17, 29) -> 3773397587873500
(18, 25) -> 5264984053703707
(19, 15) -> 5934596236453024
(20, 21) -> 3202792509191044
(21, 17) -> 5949867660133599
(22, 14) -> 5980985345945093
(23, 26) -> 3750013061015578
(24, 20) -> 4623427566244286
(25, 5) -> 5355059137105777
(26, 27) -> 4282910535867407
(27, 0) -> 5339646359107467
(28, 1) -> 5876466967406626
(29, 3) -> 4547556918090559"""

	// val results = 	"""(0, 2) -> 3031384557419344
	// 					(1, 12) -> 5618539037471932
	// 					(2, 24) -> 3935381234299563
	// 					(3, 23) -> 5058671859048545
	// 					(4, 14) -> 5902424330974795
	// 					(5, 13) -> 5970591737732243
	// 					(6, 27) -> 3928311057828342
	// 					(7, 11) -> 3826850167289095
	// 					(8, 26) -> 5739349463426131
	// 					(9, 20) -> 3291931158762464
	// 					(10, 15) -> 5429032685908353
	// 					(11, 10) -> 5354934208730446
	// 					(12, 4) -> 4689423355266347
	// 					(13, 19) -> 5290919913639559
	// 					(14, 7) -> 4277728490969330
	// 					(15, 16) -> 5334344371478679
	// 					(16, 25) -> 5918318369052091
	// 					(17, 18) -> 5335443602416753
	// 					(18, 6) -> 6209879459725537
	// 					(19, 5) -> 5885287308081362
	// 					(20, 9) -> 4091586355190660
	// 					(21, 29) -> 4914479288972144
	// 					(22, 1) -> 6211627161159312
	// 					(23, 3) -> 3862596457801873
	// 					(24, 28) -> 5523011129800072
	// 					(25, 17) -> 4843139350980810
	// 					(26, 22) -> 4090076070460680
	// 					(27, 21) -> 5780140216533294
	// 					(28, 0) -> 3706088903821428
	// 					(29, 8) -> 5277117199792027"""

	val tupleRE = """^\((\d+), (\d+)\) -> (\d+)$""".r
	val tuples = results.split("\n").map(_.trim).map{ x => x match {
			case tupleRE(x,y,z) => (
				x.toInt,
				y.toInt,
				BigDecimal(z.toLong.toDouble/10000000000000000.0).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
				) 
		}
	}
	//BigDecimal(diff.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble.toString

	tuples.foreach(println)

var scDirichlets = List[Double]()
var psDirichlets = List[Double]()

def compareTopics (tupl: (Int, Int, Double)) = {

		val scTopicNumber = tupl._1
		val	scFile = new File("data/mallet/sc30-top-words.txt")
		val scLines = Source.fromFile(scFile).getLines.toVector
		val scTopicLine = scLines(scTopicNumber)


		val psTopicNumber = tupl._2
		val psFile = new File("data/mallet/ps30-top-words.txt")
		val psLines = Source.fromFile(psFile).getLines.toVector
		val psTopicLine = psLines(psTopicNumber)

		val RE = """^\d{1,2}\s+(\d+\.\d+)\s+(\w.+)$""".r

		val RE(scParameter, scWordString) = scTopicLine
		val RE(psParameter, psWordString) = psTopicLine

		val scWords = scWordString.split("\\s+")
		val psWords = psWordString.split("\\s+")
		val wordPairs = scWords.zip(psWords)


// 		println("""\begin{table}[ht]
// \begin{center}
// \begin{tabular}{|c|c|}
// \hline 
// PS-""" + scTopicNumber + """ & NS-""" + psTopicNumber + """ \\ \hline""")
// wordPairs.take(25).foreach(tupl => println(tupl._1 + " & " + tupl._2 + """\\"""))
// println("""\hline
// \end{tabular}
// \end{center}
// \caption{\label{} Top words: PS: """ + scParameter + """; NPS: """ + psParameter + "; JSD: " + tupl._3 + """ }
// \end{table}""")
// println("\n")


scDirichlets = scParameter.toDouble::scDirichlets

psDirichlets = psParameter.toDouble::psDirichlets


	}



	def compareTopics (scTopicNumber: Int, psTopicNumber: Int) = {

		val	scFile = new File("data/mallet/trainingOutput/sc30-top-words.txt")
		val scLines = Source.fromFile(scFile).getLines.toVector
		val scTopicLine = scLines(scTopicNumber)



		val psFile = new File("data/mallet/trainingOutput/ps30-top-words.txt")
		val psLines = Source.fromFile(psFile).getLines.toVector
		val psTopicLine = psLines(psTopicNumber)

		val RE = """^\d{1,2}\s+(\d+\.\d+)\s+(\w.+)$""".r

		val RE(scParameter, scWordString) = scTopicLine
		val RE(psParameter, psWordString) = psTopicLine

		val scWords = scWordString.split("\\s+")
		val psWords = psWordString.split("\\s+")
		val wordPairs = scWords.zip(psWords)


// 		println("""\begin{table}[ht]
// \begin{center}
// \begin{tabular}{|l|rl|}
// \hline \bf PS & \bf NPS & \bf \\ \hline""")
// wordPairs.take(20).foreach(tupl => println(tupl._1 + " & " + tupl._2 + """ & \\"""))
// println("""\hline
// \end{tabular}
// \end{center}
// \caption{\label{} Top words}
// \end{table}""")



	}



	def main (args: Array[String]) {
		if (args.length == 2) compareTopics(args(0).toInt, args(1).toInt)
		else tuples.foreach(compareTopics)
		println("sc Dirichlets sum to: " + scDirichlets.sum)
		println(scDirichlets.length)
		println(scDirichlets)
		println("ps Dirichlets sum to: " + psDirichlets.sum)
		println(psDirichlets.length)
		println(psDirichlets)
	}


}


object Redo {

	def main (args: Array[String]) {

		val lines = Source.fromFile(args(0)).getLines

		val fixedLines = lines.map{line =>
			val Array(word, diff0, pro, ns, neut, proRatio0, nsRatio0) = line
				.replaceAllLiterally("""\\ \hline""", "")
				.replaceAllLiterally("""\\""", "")
				.split(" & ")
			val diff = BigDecimal(diff0.toDouble).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString
			val proRatio = BigDecimal(proRatio0.toDouble).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString
			val nsRatio = BigDecimal(nsRatio0.toDouble).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString
			//Array(word, diff, proRatio, nsRatio, pro, ns, neut).mkString(" & ")
			Array(word, diff, proRatio, nsRatio).mkString(" & ")
		}

		fixedLines.foreach(x => println(x + """ \\ \hline"""))
	}


}






object Queries {

	val QueryRE = """"[ -]*q: ?([^ ].+)""".r

	def tryGetQueryWordsFromLine (line: String) = {
			try {
				line match { case QueryRE(x) => x}
			} catch {
				case e: Throwable => {
					""
				}
			}
		}

	val lines = scala.io.Source.fromFile("data/twitterUsers/mainList.txt").getLines.filter(x=> x.startsWith("\"")&& x.contains("q:"))

	val strings: List[String] = lines.map(tryGetQueryWordsFromLine).toList.toSet.toList.sorted

	def main (args: Array[String]) {
		strings.foreach(x => if (x!="") println(x + """ \\ \hline"""))
	}
		//val strings = lines.map(x=>x match { case QueryRE(x) => x})
	//val queryWords = Source.fromFile("src/main/resources/queryWords.txt").getLines.toList.toSet
	// val queryies = strings
	// 		.map(_.split("(?: (?:AND|and|OR|or) | )"))
	// 		.toList
	// 		.flatten
	// 		.map(_.replaceAll("\"", ""))
	// 		.map(_.replaceAll("""\(""", ""))
	// 		.map(_.replaceAll("""\)""", ""))
	// 		.map(_.toLowerCase)
	// 		.toSet
}