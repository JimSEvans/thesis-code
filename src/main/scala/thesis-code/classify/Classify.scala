package thesiscode.classify

import nak.NakContext._
import nak.core._
import nak.data._
import nak.liblinear.LiblinearConfig
import nak.util.ConfusionMatrix
import java.io.File
import scala.io.Source
import thesiscode.util.Util._
import thesiscode.util.LiwcCategories
import thesiscode.classify.ClassifyUtil._
import play.api.libs.json._
import scala.util.Random


object ClassifyUtil {

/** Note: this function rids the text of URLs, 
because this is easier to do using the tweet JSON metadata. the rest of the 
processing is left to the Featurizer **/
	def fromListOfFileNames(listFile: File, takeXLines: Int, sliceMethod: String)
		(implicit codec: scala.io.Codec) = { // sliceMethod possibilities = "none", "new", "old", "random"
		// println("ALL TWEETS FUNCTION")
	    for {
	    	fileName <- Source.fromFile(listFile).getLines.toIterator.map(line => line.split("\\s+")(0))
	    } yield {

	    	val fileSource = Source.fromFile(fileName)
	    	val lines = fileSource.getLines
	    	val firstLine = lines.next
	    	val label = if (firstLine.startsWith("ps")) {"ps"} else {"sc"}

	    	val textLines = lines.map{ jsonString =>
				val statusJson = Json.parse(jsonString)
				val retweetedStatusText = (statusJson \ "retweeted_status" \ "text").asOpt[String]
				val json = if (retweetedStatusText == None) {
					statusJson
					} else {
					(statusJson \ "retweeted_status")
					}
				var text = (json \ "text").as[String]
				val urls: Seq[String] = (json \\ "url").map(_.asOpt[String]).flatten
				urls.foreach{url => 
					text = text.replaceAllLiterally(url, "")
				}
				text
	    	}.toList

	    	val num = textLines.length


	    	val fullText: String = 
		    	if (takeXLines == 0 || takeXLines >= num) {
		    		textLines.mkString("\n")
		    		} else { sliceMethod match {
		    			case "recent" => textLines.takeRight(takeXLines).mkString("\n")
		    			case "old" => textLines.take(takeXLines).mkString("\n")
		    			case "random" => {
		    				val r = new Random((num - fileName.length - textLines(0).length - firstLine.length).abs)
	    					val firstArg = r.nextInt(num - (takeXLines-1))
	    					val secondArg = firstArg + takeXLines
	    					textLines.slice(firstArg, secondArg).mkString("\n")
		    			}
		    		}
		    	}

		    			// if (randomSlice == false) {
		    			// 		textLines.takeRight(takeXLines).mkString("\n")
		    			// 	} else {
		    			// 		val r = new Random((num - fileName.length - textLines(0).length/15 - firstLine.length/4).abs)
		    			// 		val firstArg = r.nextInt(num - (takeXLines-1))
		    			// 		val secondArg = firstArg + takeXLines
		    			// 		textLines.slice(firstArg, secondArg).mkString("\n")
		    			// 	}
		    			// }



	    	fileSource.close
	    	//println (fileName + " " + fullText.split("\n").length)
	    	Example(label, fullText, fileName)
	    }

	  }






	def fromListOfFileNamesNoRetweets(listFile: File, takeXLines: Int, sliceMethod: String)
		(implicit codec: scala.io.Codec) = {
		// println("NO RETWEETS FUNCTION")
	    for {
	    	fileName <- Source.fromFile(listFile).getLines.toIterator.map(line => line.split("\\s+")(0))
	    } yield {

	    	val fileSource = Source.fromFile(fileName)
	    	val lines = fileSource.getLines
	    	val firstLine = lines.next
	    	val label = if (firstLine.startsWith("ps")) {"ps"} else {"sc"}

	    	val filtered = lines.filter{ jsonString =>
					val statusJson = Json.parse(jsonString)
					val retweetedStatusText = (statusJson \ "retweeted_status" \ "text").asOpt[String]
					retweetedStatusText == None}

			val textLines = filtered.map{ jsonString =>
					val statusJson = Json.parse(jsonString)
					val textOpt = (statusJson \ "text").asOpt[String]
					var text = if (textOpt == None) "" else textOpt.get
					val urls: Seq[String] = (statusJson \\ "url").map(_.asOpt[String]).flatten
					urls.foreach{ url => 
					text = text.replaceAllLiterally(url, "")
					}	

				text
	    	}.toList


	  		val num = textLines.length

	    	val fullText: String = 
		    	if (takeXLines == 0) {
		    		textLines.mkString("\n")
		    		} else { sliceMethod match {
		    			case "recent" => textLines.takeRight(takeXLines).mkString("\n")
		    			case "old" => textLines.take(takeXLines).mkString("\n")
		    			case "random" => {
		    				val r = new Random((num - fileName.length - textLines(0).length - firstLine.length).abs)
	    					val firstArg = r.nextInt(num - (takeXLines-1))
	    					val secondArg = firstArg + takeXLines
	    					textLines.slice(firstArg, secondArg).mkString("\n")
		    			}
		    		}
		    	}

	    	// val fullText: String = 
		    // 	if (takeXLines == 0) {
		    // 		textLines.mkString("\n")
		    // 		} else {
		    // 			if (randomSlice == false) {
		    // 					textLines.takeRight(takeXLines).mkString("\n")
		    // 				} else {
		    // 					val r = new Random((num - fileName.length - textLines(0).length/15 - firstLine.length/4).abs)
		    // 					val firstArg = r.nextInt(num - (takeXLines-1))
		    // 					val secondArg = firstArg + takeXLines
		    // 					textLines.slice(firstArg, secondArg).mkString("\n")
		    // 				}
		    // 			}


	    	fileSource.close
	    	//println (fileName + " " + fullText.split("\n").length)
	    	Example(label, fullText, fileName)
	    }
	  }








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
	    	val fullText = split.drop(1).mkString("\n")


	    	fileSource.close
	    	//println (fileName + " " + fullText.split("\n").length)
	    	Example(label, fullText, articleFile.getName)
	    }

	  }












	val stopwords1: Set[String] = Source.fromFile("src/main/resources/stopwords.txt")
		.getLines
		.map(word => word.trim)
		.toSet

	val stopwords2: Set[String] = Source.fromFile("src/main/resources/stopwords2.txt")
		.getLines
		.map(word => word.trim)
		.toSet

	val stopwords = stopwords1|stopwords2



	private def tryGetQueryWordsFromLine (line: String) = {
			try {
				line match { case QueryRE(x) => x}
			} catch {
				case e: Throwable => {
					""
				}
			}
		}
	private val lines = scala.io.Source.fromFile("data/twitterUsers/mainList.txt").getLines.filter(x=> x.startsWith("\"")&& x.contains("q:"))
	private val QueryRE = """"[ -]*q: ?([^ ].+)""".r
	private val strings: Iterator[String] = lines.map(tryGetQueryWordsFromLine)
		//val strings = lines.map(x=>x match { case QueryRE(x) => x})
	val queryWords = Source.fromFile("src/main/resources/queryWords.txt").getLines.toList.toSet
	// val queryWords = strings
	// 		.map(_.split("(?: (?:AND|and|OR|or) | )"))
	// 		.toList
	// 		.flatten
	// 		.map(_.replaceAll("\"", ""))
	// 		.map(_.replaceAll("""\(""", ""))
	// 		.map(_.replaceAll("""\)""", ""))
	// 		.map(_.toLowerCase)
	// 		.toSet


	//queryWords.toList.sortBy(x=> if (x.startsWith("#")) x.drop(1) else x).foreach(println)	









	def getHashtagFeatures (raw: String, count:Boolean): Seq[FeatureObservation[String]] = {

		val clean = cleanText(raw, true, false, true, true)
		//println("clean\n" + clean)
		val tokens = clean.split("\\s+")
		//println("tokens")
		//tokens.foreach(x=>print(x + " "))
		val hashtags = tokens.filter(_.startsWith("#")).filterNot(queryWords)
		// println("hashtags")
		// hashtags.foreach(x=>print(x + " "))
		val bares = hashtags.map(tag => tag.drop(1))

		val shorts = bares.filter(_.length <= 3).toList
			.map( charSeq => "HTshort=" + charSeq )

		val fours = bares.filter(_.length > 3)
			.flatMap(bare => bare.sliding(4).toList)
			.map( gram4 => "HT4gram=" + gram4 )

		val fives = bares.filter(_.length > 4)
			.flatMap(bare => bare.sliding(5).toList)
			.map( gram5 => "HT5gram=" + gram5 )

		var all: List[String] = shorts ++ fours ++ fives

		var features = List[FeatureObservation[String]]()

		if (count) {
			val countMap = all.groupBy(x=>x).mapValues(x=>x.length)
			features = countMap.keys.toList.map(x => new FeatureObservation(x, countMap(x)))
		} else {
			val uniq: Set[String] = all.toSet
			features = uniq.toList.map(x => new FeatureObservation(x))
			}
		// println("\nfeatures")
		// features.toSeq.foreach(println)
		features.toSeq
	}






	def getLiwcFeatures (raw: String, count: Boolean): Seq[FeatureObservation[String]] = {

		
		val featuresAsVectorOfTuples = LiwcCategories(raw)

		val nonZeroFeatures = featuresAsVectorOfTuples.filterNot(tupl => tupl._2 == 0.0)

		val liwcFeatures = 
			if (count) {
				nonZeroFeatures.map(tupl => new FeatureObservation(tupl._1, tupl._2))
			} else {
				nonZeroFeatures.map(tupl => new FeatureObservation(tupl._1))
			}
		liwcFeatures
	}




	def getUnigramFeatures (text: String, count: Boolean, stem: Boolean): Seq[FeatureObservation[String]] = {

		val cleanedText: String = cleanText(text,false,false,true,true)
		var wordArray = 
			if (stem) {cleanedText.split("\\s+").filterNot(stopwords ++ queryWords).map(word => stemmer(word))}
			else {cleanedText.split("\\s+").filterNot(stopwords ++ queryWords)}
		var features = Seq[FeatureObservation[String]]()
		if (!count) {
			wordArray = wordArray.toSet.toArray
			features = wordArray.map(tok => new FeatureObservation("uni=" + tok))
		} else {
			val m = wordArray.groupBy(x=>x).mapValues(x=>x.length)
			features = m.keys.toSeq.map(key => new FeatureObservation("uni=" + key, m(key).toDouble))
		}
		features

	}

	def getUnigramFeaturesIncludeHashtags (text: String, count: Boolean, stem: Boolean): Seq[FeatureObservation[String]] = {

		val cleanedText: String = cleanText(text,true,false,true,true)
		var wordArray = 
			if (stem) {cleanedText.split("\\s+").filterNot(stopwords ++ queryWords).map(word => stemmer(word))}
			else {cleanedText.split("\\s+").filterNot(stopwords ++ queryWords)}
		wordArray = wordArray.map{word => 
			word match {
				case word if word.startsWith("#") => word.drop(1)
				case _ => word 
				}
			}
		var features = Seq[FeatureObservation[String]]()
		if (!count) {
			wordArray = wordArray.toSet.toArray
			features = wordArray.map(tok => new FeatureObservation("uni=" + tok))
		} else {
			val m = wordArray.groupBy(x=>x).mapValues(x=>x.length)
			features = m.keys.toSeq.map(key => new FeatureObservation("uni=" + key, m(key).toDouble))
		}
		features

	}



	def getBigramFeatures (text: String, count: Boolean, stem: Boolean): Seq[FeatureObservation[String]] = {

		val basicText: String = cleanText(text,true,true,true,true)
		val lines = basicText.split("\n")
// 		// var wordsForUnigrams = Array[String]()
		val linesWithSentenceBorders = lines.map{ l =>
			var line = l.replaceAll("""([&\*\"\|\[\]\(\)\{\},'“”/\|\\]|--+|<|>)""", "")
			line = line.replaceAll("""[\.;:!\?…]""", " & ")
			line = "& " + line + " &"
			line.trim.replaceAll("\\s+", " ")
		}

		var fullText: String = linesWithSentenceBorders.mkString(" ")
		fullText = fullText.replaceAll("""\s*&\s*(?:&\s*)+""", " & ")
// 		// line = line.replaceAllIn("""((?:\.\s*){2,}|(?:;\s*){2,}|(?::\s*){2,}(?:!\s*){2,}|(?:\?\s*){2,}|(?:…\s*){2,})""", $1.trim.split("\\s+")(0)))
		val words = fullText.split("\\s+").map{ word => word match {
			case word if queryWords(word) => "[-queryword-]"
			case word if (word.startsWith("#") && word.length >= 2) => "[-hashtag-]"
			case _ => if (stem) stemmer(word) else word
			}
		}
		val arrayOfTwos: Array[Array[String]] = words.sliding(2).toArray
		val allBigrams: Array[String] = arrayOfTwos.map{ array =>
			array.mkString("_")
		}
		var bigramFeatures = Seq[FeatureObservation[String]]()

		if (!count) {
			val uniq = allBigrams.toSet.toArray
			bigramFeatures = uniq.map(tok => new FeatureObservation("bi=" + tok))
		} else {
			val bMap = allBigrams.groupBy(x=>x).mapValues(x=>x.length)
			bigramFeatures = bMap.keys.toSeq.map(key => new FeatureObservation("bi=" + key, bMap(key).toDouble))

		}
		bigramFeatures
	}










	def main(args: Array[String]) {
		//getHashtagFeatures("hey #there dude \nwhat's up #7g4eva", false).foreach(println)
		val dir = new File("data/Articles/data2")
		var counter = 0
		var numFiles = 0
		var bad = List[String]()
		val files = dir.listFiles.filterNot(_.isHidden).filter(_.isFile)
		files.foreach{ f =>
			println("")
			println(f.getName)
			try {
				val x = Source.fromFile(f).getLines.next
				println("ok")
				counter += 1
				numFiles += 1 
				} catch {
					case t: Throwable => {
						numFiles += 1
						bad = f.getName::bad
						println("*** ERROR ***: " + f.getName + "********************************************************************************")
					}
				}
		}

		println("")
		println("good files = " + counter)
		println("")
		println("bad files = " + bad.length)
		println("")
		println("all files = " + numFiles)
		bad.foreach(println)

	}


} // object ClassifyUtil







/** featurize by putting the json document File, which is like 
label
tweetJSON
tweetJSON
etc

minus its first line (i.e.
tweetJSON
tweetJSON
)
into the apply method
**/

class BlankFeaturizer extends Featurizer[String, String] {
	def apply(raw: String) = Seq[FeatureObservation[String]]()
}

class MainFeaturizer (

	// stopwords: Set[String],
	// queryWords: Set[String],
	uni: Boolean,
	uniCount: Boolean,
	uniStem: Boolean,
	bi: Boolean,
	biCount: Boolean,
	biStem: Boolean,
	liwc: Boolean,
	liwcCount: Boolean,
	hashtags: Boolean,
	hashtagsCount: Boolean,
	unihashtag: Boolean

	) extends Featurizer[String, String] {


	def apply(raw: String) = {

		val text = cleanText(raw, true, true, false, false)

		// println(text)

		var features = Seq[FeatureObservation[String]]()

		if (unihashtag) {
			features = getUnigramFeaturesIncludeHashtags(text, uniCount, uniStem) ++ features
		}

		if (uni) {
			features = getUnigramFeatures(text, uniCount, uniStem) ++ features
		}

		if (bi) {
			features = getBigramFeatures(text, biCount, biStem) ++ features
		}

		if (hashtags) {
			features = getHashtagFeatures(text, hashtagsCount) ++ features
		}

		if (liwc) {
			features = getLiwcFeatures(text, liwcCount) ++ features
		}


		// features.foreach(println)
		features
		
	}
}











































object UserClassify {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig
  import nak.util.ConfusionMatrix
  import thesiscode.classify.ClassifyUtil._






  def main(args: Array[String]) {  // train/full/all  dev/main/health  uni-bi-ht-hashing take:50-random-orig none
  	val trainOn = args(0) //e.g.  train/full/all
  	val evalOn = args(1) // e.g. dev/main/health
  	val options = args(2) // e.g. hyphen-separated list of options. examples include uni-c=0.5; uni-ht; bi; bi-uni; bi-uni-ht-hashing:30000 (NB: ht => use hashtag features, hashing = use hashing trick). 
	val trainOptions = args(3) // e.g. none; take:50:old-orig; take:1:random
	val evalOptions = args(4) // e.g. none; take:50:recent-orig; take:1:random





	val nameForThisClassifier = {
		val unchanged: String = trainOn + "_" + options + "_" + trainOptions
		unchanged.replaceAll(":", ",").replaceAll("/", ";")
	}
	println("name for this classifier")
	println(nameForThisClassifier)


	val classifiersDir = new File("savedClassifiers")
	val pastClassifiers = classifiersDir.listFiles.toList.filter(_.isFile).filterNot(_.isHidden)
	//pastClassifiers.foreach(x=>println(x.getName))
	val classifiersThatMatchCurrentSpecifications: List[File] = pastClassifiers.filter{classifierFile => //e.g. train;full;all_uni-bi-ht-c=hashing:500000_take,50,random-orig
			val classifierName = classifierFile.getName
			val optionGroups: Array[String] = classifierName.split("_")
			val optionGroupsAsSets: Array[Set[String]] = optionGroups.map(groupString => groupString.split("-").toSet)
			val Array(trainedOn, genSpecs, trainSpecs) = optionGroupsAsSets

			val currentTrainOn = trainOn.replaceAll("/", ";").split("-").toSet
			//println("current " + currentTrainOn)
			//println("old " + trainedOn)
			val currentGenOptions = options.replaceAll(":",",").split("-").toSet
			val currentTrainOptions = trainOptions.replaceAll(":",",").split("-").toSet

			(trainedOn == currentTrainOn && genSpecs == currentGenOptions && trainSpecs == currentTrainOptions)
			//file.getName == nameForThisClassifier
		}
	println("num matching classifiers: " + classifiersThatMatchCurrentSpecifications.length)







  	var opts = options.split("-").toSet - ("none")

  	var evalOpts = evalOptions.split("-").toSet - ("none")
  	var trainOpts = trainOptions.split("-").toSet - ("none")


  	val basePath = "data/corpusJSON/dataSplitsDir/"
  	val evalFilePath = basePath + evalOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/health
  	val evalFile = new File(evalFilePath)


  	val classifier = if (classifiersThatMatchCurrentSpecifications.length == 1) {
  		println("Loading classifier from file...")
  		loadClassifier(classifiersThatMatchCurrentSpecifications(0).getPath)
  	} else {

  		println("Training new classifier...")

	  	val trainFilePath = basePath + trainOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/all
	  	val trainFile = new File(trainFilePath)

	    val hashtags = opts("ht")
	    val hashtagsCount = opts("htCount")
	    val uni = opts("uni")
	    val uniCount = opts("uniCount")
		val uniStem = opts("uniStem")
	    val bi = opts("bi")	
	    val biCount = opts("biCount")
		val biStem = opts("biStem")
	    val liwc = opts("liwc")
		val liwcCount = opts("liwcCount")
		val unihashtag = opts("unihashtag")



		val featurizer = new MainFeaturizer(uni, uniCount, uniStem, bi, biCount, biStem, liwc, liwcCount, hashtags, hashtagsCount, unihashtag)




	    var trainTakeXLines = 0
	    var trainSliceMethod = "recent"

	    val trainTakeOptList = trainOpts.toList.filter(opt=>opt.contains("take"))

	    if (trainTakeOptList.length == 1) {
	    	val trainTakeOpt = trainTakeOptList(0)
	    	//println("trainTakeOpt should be 1: " + trainTakeOpt)
	    	trainOpts = trainOpts - trainTakeOpt
	    	trainTakeXLines = trainTakeOpt.split(":")(1).toInt
	    	trainSliceMethod = trainTakeOpt.split(":")(2)
	    }






	    // Train
	    print("Training... ")



	    var c = 1.0

	    val cOptList = opts.toList.filter(opt=>opt.contains("c="))   //e.g. "c=5.0"
	    if (cOptList.length == 1) {
	    	val cOpt = cOptList(0)
	    	opts = opts - cOpt
	    	c = cOpt.split("=")(1).toDouble
	    }

	    println(c)

	    val trainingExamples = 
	    	if (trainOpts("orig")) {
	    		trainOpts = trainOpts - "orig"
	    		fromListOfFileNamesNoRetweets(trainFile, trainTakeXLines, trainSliceMethod).toList
	    	} else {
	    		fromListOfFileNames(trainFile, trainTakeXLines, trainSliceMethod).toList
	    	}


	    val config = LiblinearConfig(cost=c,eps=0.01)




	    opts = opts - "ht"

		val classifier = {
			val hashingOptList = opts.toList.filter(opt=>opt.contains("hashing"))
			if (hashingOptList.length==1) {
				val hashingOpt = hashingOptList(0)
				val numFeatures = {
					val arr = hashingOpt.split(":")
					if (arr.length==2) {
						arr(1).toInt
					} else {
						50000
					}
				}
				opts = opts - hashingOpt
				trainClassifierHashed(config, featurizer, trainingExamples, numFeatures)
			} else {
				trainClassifier(config, featurizer, trainingExamples)
			}
		}


	classifier


  	}


  	if (classifiersThatMatchCurrentSpecifications.length == 0) {
  		saveClassifier(classifier, ("savedClassifiers/" + nameForThisClassifier))
  	}



	opts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))


	trainOpts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))



    println("done.")





    // Evaluate
    println("Evaluating...")





    var evalTakeXLines = 0
    var evalSliceMethod = "recent"

    val evalTakeOptList = evalOpts.toList.filter(opt=>opt.contains("take"))

    if (evalTakeOptList.length == 1) {
    	val evalTakeOpt = evalTakeOptList(0)
    	//println("evalTakeOpt should be 1: " + evalTakeOpt)
    	evalOpts = evalOpts - evalTakeOpt
    	evalTakeXLines = evalTakeOpt.split(":")(1).toInt
    	evalSliceMethod = evalTakeOpt.split(":")(2)
    }


    val evalExamples = 
    	if (evalOpts("orig")) {
    		evalOpts = evalOpts - "orig"
    		fromListOfFileNamesNoRetweets(evalFile, evalTakeXLines, evalSliceMethod).toList
    	} else if (evalOpts("articles")) {
    		labeledArticles().toList
    	} else {
    		fromListOfFileNames(evalFile, evalTakeXLines, evalSliceMethod).toList
    	}


	evalOpts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))


	val comparisons1 = for (ex <- evalExamples) yield
      (ex.label, classifier.predict(ex.features), ex.features)
    val (goldLabels, predictions, inputs) = comparisons1.unzip3
    println(ConfusionMatrix(goldLabels, predictions, inputs))



 //    if (evalOpts("predict")) {

	//     val comparisons2 = for (ex <- evalExamples) yield
	//     	(ex.label, ex.id, classifier.labels.zip(classifier.evalRaw(ex.features)), ex.features)
	//     comparisons2.foreach{ comparison => 
	//     	val tuplOfArrays = comparison._3
	//     	val id = comparison._2
	//     	val goldLabel = comparison._1
	//     	println("\n" + id + " " + goldLabel)
	//     	tuplOfArrays.foreach{ tupl => 
	//     		println(tupl._1 + " " + tupl._2)
	//     	}
	//     }
	// }
	if (evalOpts("predict")) {

		var errors = List[(String, String, String, Double)]()

	    val comparisons2 = for (ex <- evalExamples) yield
	    	(ex.label, ex.id, classifier.labels.zip(classifier.evalRaw(ex.features)), ex.features)
	    comparisons2.foreach{ comparison => 
	    	val arrayOfTuples = comparison._3.sortBy(tupl => tupl._2).reverse
	    	val predictedLabel = arrayOfTuples(0)._1
	    	val confidence = arrayOfTuples(0)._2
	    	val id = comparison._2
	    	val goldLabel = comparison._1
	    	val header = if (goldLabel == predictedLabel) "correct" else "INCORRECT*********************************"
	    	if (header != "correct") errors = (id, goldLabel, predictedLabel, confidence)::errors
	    	println("\n" + header)
	    	println(id + " " + goldLabel)
	    	arrayOfTuples.foreach{ tupl => 
	    		println(tupl._1 + " " + tupl._2)
	    	}
	    }
	    errors.foreach(println)
	}

  }
  
}























































// object ArticleClassify {

//   import nak.NakContext._
//   import nak.core._
//   import nak.data._
//   import nak.liblinear.LiblinearConfig
//   import nak.util.ConfusionMatrix
//   import thesiscode.classify.ClassifyUtil._






//   def main(args: Array[String]) {  // train/full/all  dev/main/health  uni-bi-ht-hashing take:50-random-orig none
//   	val trainOn = args(0) //e.g.  train/full/all
//   	val evalOn = args(1) // e.g. dev/main/health
//   	val options = args(2) // e.g. hyphen-separated list of options. examples include uni-c=0.5; uni-ht; bi; bi-uni; bi-uni-ht-hashing:30000 (NB: ht => use hashtag features, hashing = use hashing trick). 
// 	val trainOptions = args(3) // e.g. none; take:50:old-orig; take:1:random
// 	val evalOptions = args(4) // e.g. none; take:50:recent-orig; take:1:random





// 	val nameForThisClassifier = {
// 		val unchanged: String = trainOn + "_" + options + "_" + trainOptions
// 		unchanged.replaceAll(":", ",").replaceAll("/", ";")
// 	}
// 	println("name for this classifier")
// 	println(nameForThisClassifier)


// 	val classifiersDir = new File("savedClassifiers")
// 	val pastClassifiers = classifiersDir.listFiles.toList.filter(_.isFile).filterNot(_.isHidden)
// 	//pastClassifiers.foreach(x=>println(x.getName))
// 	val classifiersThatMatchCurrentSpecifications: List[File] = pastClassifiers.filter{classifierFile => //e.g. train;full;all_uni-bi-ht-hashing_take,50,random-orig
// 			val classifierName = classifierFile.getName
// 			val optionGroups: Array[String] = classifierName.split("_")
// 			val optionGroupsAsSets: Array[Set[String]] = optionGroups.map(groupString => groupString.split("-").toSet)
// 			val Array(trainedOn, genSpecs, trainSpecs) = optionGroupsAsSets

// 			val currentTrainOn = trainOn.replaceAll("/", ";").split("-").toSet
// 			//println("current " + currentTrainOn)
// 			//println("old " + trainedOn)
// 			val currentGenOptions = options.replaceAll(":",",").split("-").toSet
// 			val currentTrainOptions = trainOptions.replaceAll(":",",").split("-").toSet

// 			(trainedOn == currentTrainOn && genSpecs == currentGenOptions && trainSpecs == currentTrainOptions)
// 			//file.getName == nameForThisClassifier
// 		}
// 	println("num matching classifiers: " + classifiersThatMatchCurrentSpecifications.length)







//   	var opts = options.split("-").toSet - ("none")

//   	var evalOpts = evalOptions.split("-").toSet - ("none")
//   	var trainOpts = trainOptions.split("-").toSet - ("none")


//   	val basePath = "data/corpusJSON/dataSplitsDir/"
//   	val evalFilePath = basePath + evalOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/health
//   	val evalFile = new File(evalFilePath)


//   	val classifier = if (classifiersThatMatchCurrentSpecifications.length == 1) {
//   		println("Loading classifier from file...")
//   		loadClassifier(classifiersThatMatchCurrentSpecifications(0).getPath)
//   	} else {

//   		println("Training new classifier...")

// 	  	val trainFilePath = basePath + trainOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/all
// 	  	val trainFile = new File(trainFilePath)

// 	    val hashtags = opts("ht")
// 	    val hashtagsCount = opts("htCount")
// 	    val uni = opts("uni")
// 	    val uniCount = opts("uniCount")
// 		val uniStem = opts("uniStem")
// 	    val bi = opts("bi")	
// 	    val biCount = opts("biCount")
// 		val biStem = opts("biStem")
// 	    val liwc = opts("liwc")
// 		val liwcCount = opts("liwcCount")




// 		val featurizer = new MainFeaturizer(uni, uniCount, uniStem, bi, biCount, biStem, liwc, liwcCount, hashtags, hashtagsCount)




// 	    var trainTakeXLines = 0
// 	    var trainSliceMethod = "recent"

// 	    val trainTakeOptList = trainOpts.toList.filter(opt=>opt.contains("take"))

// 	    if (trainTakeOptList.length == 1) {
// 	    	val trainTakeOpt = trainTakeOptList(0)
// 	    	//println("trainTakeOpt should be 1: " + trainTakeOpt)
// 	    	trainOpts = trainOpts - trainTakeOpt
// 	    	trainTakeXLines = trainTakeOpt.split(":")(1).toInt
// 	    	trainSliceMethod = trainTakeOpt.split(":")(2)
// 	    }






// 	    // Train
// 	    print("Training... ")



// 	    var c = 1.0

// 	    val cOptList = opts.toList.filter(opt=>opt.contains("c="))   //e.g. "c=5.0"
// 	    if (cOptList.length == 1) {
// 	    	val cOpt = cOptList(0)
// 	    	opts = opts - cOpt
// 	    	c = cOpt.split("=")(1).toDouble
// 	    }

// 	    println(c)

// 	    val trainingExamples = 
// 	    	if (trainOpts("orig")) {
// 	    		trainOpts = trainOpts - "orig"
// 	    		fromListOfFileNamesNoRetweets(trainFile, trainTakeXLines, trainSliceMethod).toList
// 	    	} else {
// 	    		fromListOfFileNames(trainFile, trainTakeXLines, trainSliceMethod).toList
// 	    	}


// 	    val config = LiblinearConfig(cost=c,eps=0.01)




// 	    opts = opts - "ht"

// 		val classifier = {
// 			val hashingOptList = opts.toList.filter(opt=>opt.contains("hashing"))
// 			if (hashingOptList.length==1) {
// 				val hashingOpt = hashingOptList(0)
// 				val numFeatures = {
// 					val arr = hashingOpt.split(":")
// 					if (arr.length==2) {
// 						arr(1).toInt
// 					} else {
// 						50000
// 					}
// 				}
// 				opts = opts - hashingOpt
// 				trainClassifierHashed(config, featurizer, trainingExamples, numFeatures)
// 			} else {
// 				trainClassifier(config, featurizer, trainingExamples)
// 			}
// 		}


// 	classifier


//   	}


//   	if (classifiersThatMatchCurrentSpecifications.length == 0) {
//   		saveClassifier(classifier, ("savedClassifiers/" + nameForThisClassifier))
//   	}



// 	opts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))


// 	trainOpts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))



//     println("done.")





//     // Evaluate
//     println("Evaluating...")





//     var evalTakeXLines = 0
//     var evalSliceMethod = "recent"

//     val evalTakeOptList = evalOpts.toList.filter(opt=>opt.contains("take"))

//     if (evalTakeOptList.length == 1) {
//     	val evalTakeOpt = evalTakeOptList(0)
//     	//println("evalTakeOpt should be 1: " + evalTakeOpt)
//     	evalOpts = evalOpts - evalTakeOpt
//     	evalTakeXLines = evalTakeOpt.split(":")(1).toInt
//     	evalSliceMethod = evalTakeOpt.split(":")(2)
//     }


//     val evalExamples = labeledArticles()


// 	evalOpts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))


//     val comparisons = for (ex <- evalExamples) yield
//       (ex.label, classifier.predict(ex.features), ex.features)
//     val (goldLabels, predictions, inputs) = comparisons.unzip3
//     println(ConfusionMatrix(goldLabels, predictions, inputs))

//   }
  
// }















































object BaselineClassify {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig
  import nak.util.ConfusionMatrix
  import thesiscode.classify.ClassifyUtil._




  def main(args: Array[String]) {  // train/full/all  dev/main/health  uni-bi-ht-hashing take:50-random-orig none
  	val trainOn = args(0) //e.g. train/all
  	val evalOn = args(1) // e.g. dev/health
  	val options = args(2) // e.g. hyphen-separated list of options. examples include uni-c=0.5; uni-ht; bi; bi-uni; bi-uni-ht-hashing:30000 (NB: ht => use hashtag features, hashing = use hashing trick). 
	val trainOptions = args(3) // e.g. none; take:50:old-orig; take:1:random
	val evalOptions = args(4) // e.g. none; take:50:recent-orig; take:1:random



  	var opts = options.split("-").toSet - ("none")

  	var evalOpts = evalOptions.split("-").toSet - ("none")
  	var trainOpts = trainOptions.split("-").toSet - ("none")


  	val basePath = "data/corpusJSON/dataSplitsDir/"
  	val trainFilePath = basePath + trainOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/all
  	val evalFilePath = basePath + evalOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/health
  	val trainFile = new File(trainFilePath)
  	val evalFile = new File(evalFilePath)



    val hashtags = opts("ht")
    val hashtagsCount = opts("htCount")
    val uni = opts("uni")
    val uniCount = opts("uniCount")
	val uniStem = opts("uniStem")
    val bi = opts("bi")	
    val biCount = opts("biCount")
	val biStem = opts("biStem")
    val liwc = opts("liwc")
	val liwcCount = opts("liwcCount")


    val featurizer = new BlankFeaturizer


    var trainTakeXLines = 0
    var trainSliceMethod = "recent"

    val trainTakeOptList = trainOpts.toList.filter(opt=>opt.contains("take"))

    if (trainTakeOptList.length == 1) {
    	val trainTakeOpt = trainTakeOptList(0)
    	//println("trainTakeOpt should be 1: " + trainTakeOpt)
    	trainOpts = trainOpts - trainTakeOpt
    	trainTakeXLines = trainTakeOpt.split(":")(1).toInt
    	trainSliceMethod = trainTakeOpt.split(":")(2)
    }


    // Train
    print("Training... ")

    var c = 1.0

    val cOptList = opts.toList.filter(opt=>opt.contains("c="))   //e.g. "c=5.0"
    if (cOptList.length == 1) {
    	val cOpt = cOptList(0)
    	opts = opts - cOpt
    	c = cOpt.split("=")(1).toDouble
    }

    println(c)

    val trainingExamples = 
    	if (trainOpts("orig")) {
    		trainOpts = trainOpts - "orig"
    		fromListOfFileNamesNoRetweets(trainFile, trainTakeXLines, trainSliceMethod).toList
    	} else {
    		fromListOfFileNames(trainFile, trainTakeXLines, trainSliceMethod).toList
    	}



    val trainingLabels = 
    	trainingExamples
    		.map{
	    		example => example.label
	    		}
	    	.groupBy(x => x)
	    	.mapValues(x => x.length)
	    	.toList

	val mostCommonLabel = trainingLabels
	    	.sortBy(x=>x._2)
	    	.reverse(0)._1
	    	// .maxBy(x => x._2)

	trainingLabels.foreach(println)
	println(mostCommonLabel)



    println("done.")





    // Evaluate
    println("Evaluating...")





    var evalTakeXLines = 0
    var evalSliceMethod = "recent"

    val evalTakeOptList = evalOpts.toList.filter(opt=>opt.contains("take"))

    if (evalTakeOptList.length == 1) {
    	val evalTakeOpt = evalTakeOptList(0)
    	//println("evalTakeOpt should be 1: " + evalTakeOpt)
    	evalOpts = evalOpts - evalTakeOpt
    	evalTakeXLines = evalTakeOpt.split(":")(1).toInt
    	evalSliceMethod = evalTakeOpt.split(":")(2)
    }


    val evalExamples = 
    	if (evalOpts("orig")) {
    		evalOpts = evalOpts - "orig"
    		fromListOfFileNamesNoRetweets(evalFile, evalTakeXLines, evalSliceMethod).toList
    	} else if (evalOpts("articles")) {
    		labeledArticles().toList
    	} else {
    		fromListOfFileNames(evalFile, evalTakeXLines, evalSliceMethod).toList
    	}


	// evalOpts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))


	    val comparisons = for (ex <- evalExamples) yield
	      (ex.label, mostCommonLabel, ex.features)
	    val (goldLabels, predictions, inputs) = comparisons.unzip3
	    println(ConfusionMatrix(goldLabels, predictions, inputs))

  }
  
}






















































// object UserClassify2 {

//   import nak.NakContext._
//   import nak.core._
//   import nak.data._
//   import nak.liblinear.LiblinearConfig
//   import nak.util.ConfusionMatrix
//   import thesiscode.classify.ClassifyUtil._






//   def main(args: Array[String]) {  // train/full/all  dev/main/health  uni-bi-ht-hashing take:50-random-orig none
//   	val trainOn = args(0) //e.g.  train/full/all
//   	val evalOn = args(1) // e.g. dev/main/health
//   	val options = args(2) // e.g. hyphen-separated list of options. examples include uni-c=0.5; uni-ht; bi; bi-uni; bi-uni-ht-hashing:30000 (NB: ht => use hashtag features, hashing = use hashing trick). 
// 	val trainOptions = args(3) // e.g. none; take:50:old-orig; take:1:random
// 	val evalOptions = args(4) // e.g. none; take:50:recent-orig; take:1:random



//   	var opts = options.split("-").toSet - ("none")

//   	var evalOpts = evalOptions.split("-").toSet - ("none")
//   	var trainOpts = trainOptions.split("-").toSet - ("none")


//   	val basePath = "data/corpusJSON/dataSplitsDir/"
//   	val trainFilePath = basePath + trainOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/all
//   	val evalFilePath = basePath + evalOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/health
//   	val trainFile = new File(trainFilePath)
//   	val evalFile = new File(evalFilePath)








//     // val queryWords = ClassifyUtil.queryWords
//     // val stopwords = ClassifyUtil.stopwords

//     //val count = opts("count")
//     val hashtags = opts("ht")
//     val hashtagsCount = opts("htCount")
//     val uni = opts("uni")
//     val uniCount = opts("uniCount")
// 	val uniStem = opts("uniStem")
//     val bi = opts("bi")	
//     val biCount = opts("biCount")
// 	val biStem = opts("biStem")
//     val liwc = opts("liwc")
// 	val liwcCount = opts("liwcCount")


//     // val featurizer = {
//     // 	if (opts("bi")) {
//     // 		opts = opts - "bi"
//     // 		new BigramFeaturizer(stopwords, queryWords, count, liwc, hashtag, false, false, uni)
//     // 	} else {
//     // 		if (opts("uni")) {opts = opts - "uni"}
//     // 		new UnigramFeaturizer(stopwords, queryWords, count, liwc, hashtag, false, false)
//     // 	}	
//     // }

// 	// uni: Boolean,
// 	// uniCount: Boolean,
// 	// uniStem: Boolean,
// 	// bi: Boolean,
// 	// biCount: Boolean,
// 	// biStem: Boolean,
// 	// liwc: Boolean,
// 	// liwcCount: Boolean,
// 	// hashtags: Boolean,
// 	// hashtagsCount: Boolean

//     val featurizer = new MainFeaturizer(uni, uniCount, uniStem, bi, biCount, biStem, liwc, liwcCount, hashtags, hashtagsCount)




//     var trainTakeXLines = 0
//     var trainSliceMethod = "recent"

//     val trainTakeOptList = trainOpts.toList.filter(opt=>opt.contains("take"))

//     if (trainTakeOptList.length == 1) {
//     	val trainTakeOpt = trainTakeOptList(0)
//     	//println("trainTakeOpt should be 1: " + trainTakeOpt)
//     	trainOpts = trainOpts - trainTakeOpt
//     	trainTakeXLines = trainTakeOpt.split(":")(1).toInt
//     	trainSliceMethod = trainTakeOpt.split(":")(2)
//     }


//     // Train
//     print("Training... ")



//     var c = 1.0

//     val cOptList = opts.toList.filter(opt=>opt.contains("c="))   //e.g. "c=5.0"
//     if (cOptList.length == 1) {
//     	val cOpt = cOptList(0)
//     	opts = opts - cOpt
//     	c = cOpt.split("=")(1).toDouble
//     }

//     println(c)

//     val trainingExamples = 
//     	if (trainOpts("orig")) {
//     		trainOpts = trainOpts - "orig"
//     		fromListOfFileNamesNoRetweets(trainFile, trainTakeXLines, trainSliceMethod).toList
//     	} else {
//     		fromListOfFileNames(trainFile, trainTakeXLines, trainSliceMethod).toList
//     	}


//     val config = LiblinearConfig(cost=c,eps=0.01)




//     opts = opts - ("count", "ht")

// 	val classifier = {
// 		val hashingOptList = opts.toList.filter(opt=>opt.contains("hashing"))
// 		if (hashingOptList.length==1) {
// 			val hashingOpt = hashingOptList(0)
// 			val numFeatures = {
// 				val arr = hashingOpt.split(":")
// 				if (arr.length==2) {
// 					arr(1).toInt
// 				} else {
// 					50000
// 				}
// 			}
// 			opts = opts - hashingOpt
// 			trainClassifierHashed(config, featurizer, trainingExamples, numFeatures)
// 		} else {
// 			trainClassifier(config, featurizer, trainingExamples)
// 		}
// 	}



// 	opts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))


// 	trainOpts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))



//     println("done.")





//     // Evaluate
//     println("Evaluating...")





//     var evalTakeXLines = 0
//     var evalSliceMethod = "recent"

//     val evalTakeOptList = evalOpts.toList.filter(opt=>opt.contains("take"))

//     if (evalTakeOptList.length == 1) {
//     	val evalTakeOpt = evalTakeOptList(0)
//     	//println("evalTakeOpt should be 1: " + evalTakeOpt)
//     	evalOpts = evalOpts - evalTakeOpt
//     	evalTakeXLines = evalTakeOpt.split(":")(1).toInt
//     	evalSliceMethod = evalTakeOpt.split(":")(2)
//     }


//     //val howManyLinesIterator = Iterator(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,1000,1200)
//     val howManyLinesIterator = Iterator(200,300,400,500,1000,1200)
//     var howManyLines = howManyLinesIterator.next

//     println("taking: " + howManyLines)

//     var evalExamples = fromListOfFileNames(evalFile, howManyLines, evalSliceMethod).toList

//     var comparisons = for (ex <- evalExamples) yield
//       (ex.label, classifier.predict(ex.features), ex.features)

//     var tupl_GoldLabels_predictions_inputs = comparisons.unzip3
//     //(goldLabels, predictions, inputs) = comparisons.unzip3
//     var goldLabels = tupl_GoldLabels_predictions_inputs._1
//     var predictions = tupl_GoldLabels_predictions_inputs._2
//     var inputs = tupl_GoldLabels_predictions_inputs._3

//     println(ConfusionMatrix(goldLabels, predictions, inputs))


//     while (howManyLinesIterator.hasNext) {
//     	howManyLines = howManyLinesIterator.next
//     	println("taking: " + howManyLines)
//     	evalExamples = fromListOfFileNames(evalFile, howManyLines, evalSliceMethod).toList
//     	comparisons = for (ex <- evalExamples) yield
//     	(ex.label, classifier.predict(ex.features), ex.features)

//     	tupl_GoldLabels_predictions_inputs = comparisons.unzip3

// 	    goldLabels = tupl_GoldLabels_predictions_inputs._1
// 	    predictions = tupl_GoldLabels_predictions_inputs._2
// 	    inputs = tupl_GoldLabels_predictions_inputs._3

//       	//(goldLabels, predictions, inputs) = comparisons.unzip3

//       	println(ConfusionMatrix(goldLabels, predictions, inputs))
//     }

//   }
  
// }





















// object UserClassify3 {

//   import nak.NakContext._
//   import nak.core._
//   import nak.data._
//   import nak.liblinear.LiblinearConfig
//   import nak.util.ConfusionMatrix
//   import thesiscode.classify.ClassifyUtil._






//   def main(args: Array[String]) {  // train/full/all  dev/main/health  uni-bi-ht-hashing take:50-random-orig none
//   	val trainOn = args(0) //e.g.  train/full/all
//   	val evalOn = args(1) // e.g. dev/main/health
//   	val options = args(2) // e.g. hyphen-separated list of options. examples include uni-c=0.5; uni-ht; bi; bi-uni; bi-uni-ht-hashing:30000 (NB: ht => use hashtag features, hashing = use hashing trick). 
// 	val trainOptions = args(3) // e.g. none; take:50:old-orig; take:1:random
// 	val evalOptions = args(4) // e.g. none; take:50:recent-orig; take:1:random





// 	val nameForThisClassifier = {
// 		val unchanged: String = trainOn + "_" + options + "_" + trainOptions
// 		unchanged.replaceAll(":", ",").replaceAll("/", ";")
// 	}
// 	println("name for this classifier")
// 	println(nameForThisClassifier)


// 	val classifiersDir = new File("savedClassifiers")
// 	val pastClassifiers = classifiersDir.listFiles.toList.filter(_.isFile).filterNot(_.isHidden)
// 	pastClassifiers.foreach(x=>println(x.getName))
// 	val classifiersThatMatchCurrentSpecifications: List[File] = pastClassifiers.filter{classifierFile => //e.g. train;full;all_uni-bi-ht-hashing_take,50,random-orig
// 			val classifierName = classifierFile.getName
// 			val optionGroups: Array[String] = classifierName.split("_")
// 			val optionGroupsAsSets: Array[Set[String]] = optionGroups.map(groupString => groupString.split("-").toSet)
// 			val Array(trainedOn, genSpecs, trainSpecs) = optionGroupsAsSets

// 			val currentTrainOn = trainOn.replaceAll("/", ";").split("-").toSet
// 			println("current " + currentTrainOn)
// 			println("old " + trainedOn)
// 			val currentGenOptions = options.replaceAll(":",",").split("-").toSet
// 			val currentTrainOptions = trainOptions.replaceAll(":",",").split("-").toSet

// 			(trainedOn == currentTrainOn && genSpecs == currentGenOptions && trainSpecs == currentTrainOptions)
// 			//file.getName == nameForThisClassifier
// 		}
// 	println("num matching classifiers: " + classifiersThatMatchCurrentSpecifications.length)







//   	var opts = options.split("-").toSet - ("none")

//   	var evalOpts = evalOptions.split("-").toSet - ("none")
//   	var trainOpts = trainOptions.split("-").toSet - ("none")


//   	val basePath = "data/corpusJSON/dataSplitsDir/"
//   	val evalFilePath = basePath + evalOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/health
//   	val evalFile = new File(evalFilePath)


//   	val classifier = if (classifiersThatMatchCurrentSpecifications.length == 1) {
//   		println("Loading classifier from file...")
//   		loadClassifier(classifiersThatMatchCurrentSpecifications(0).getPath)
//   	} else {

//   		println("Training new classifier...")

// 	  	val trainFilePath = basePath + trainOn + ".txt" // e.g. data/corpusJSON/dataSplitsDir/train/full/all
// 	  	val trainFile = new File(trainFilePath)

// 	    val hashtags = opts("ht")
// 	    val hashtagsCount = opts("htCount")
// 	    val uni = opts("uni")
// 	    val uniCount = opts("uniCount")
// 		val uniStem = opts("uniStem")
// 	    val bi = opts("bi")	
// 	    val biCount = opts("biCount")
// 		val biStem = opts("biStem")
// 	    val liwc = opts("liwc")
// 		val liwcCount = opts("liwcCount")




// 		val featurizer = new MainFeaturizer(uni, uniCount, uniStem, bi, biCount, biStem, liwc, liwcCount, hashtags, hashtagsCount)




// 	    var trainTakeXLines = 0
// 	    var trainSliceMethod = "recent"

// 	    val trainTakeOptList = trainOpts.toList.filter(opt=>opt.contains("take"))

// 	    if (trainTakeOptList.length == 1) {
// 	    	val trainTakeOpt = trainTakeOptList(0)
// 	    	//println("trainTakeOpt should be 1: " + trainTakeOpt)
// 	    	trainOpts = trainOpts - trainTakeOpt
// 	    	trainTakeXLines = trainTakeOpt.split(":")(1).toInt
// 	    	trainSliceMethod = trainTakeOpt.split(":")(2)
// 	    }






// 	    // Train
// 	    print("Training... ")



// 	    var c = 1.0

// 	    val cOptList = opts.toList.filter(opt=>opt.contains("c="))   //e.g. "c=5.0"
// 	    if (cOptList.length == 1) {
// 	    	val cOpt = cOptList(0)
// 	    	opts = opts - cOpt
// 	    	c = cOpt.split("=")(1).toDouble
// 	    }

// 	    println(c)

// 	    val trainingExamples = 
// 	    	if (trainOpts("orig")) {
// 	    		trainOpts = trainOpts - "orig"
// 	    		fromListOfFileNamesNoRetweets(trainFile, trainTakeXLines, trainSliceMethod).toList
// 	    	} else {
// 	    		fromListOfFileNames(trainFile, trainTakeXLines, trainSliceMethod).toList
// 	    	}


// 	    val config = LiblinearConfig(cost=c,eps=0.01)




// 	    opts = opts - "ht"

// 		val classifier = {
// 			val hashingOptList = opts.toList.filter(opt=>opt.contains("hashing"))
// 			if (hashingOptList.length==1) {
// 				val hashingOpt = hashingOptList(0)
// 				val numFeatures = {
// 					val arr = hashingOpt.split(":")
// 					if (arr.length==2) {
// 						arr(1).toInt
// 					} else {
// 						50000
// 					}
// 				}
// 				opts = opts - hashingOpt
// 				trainClassifierHashed(config, featurizer, trainingExamples, numFeatures)
// 			} else {
// 				trainClassifier(config, featurizer, trainingExamples)
// 			}
// 		}


// 	classifier


//   	}


//   	if (classifiersThatMatchCurrentSpecifications.length == 0) {
//   		saveClassifier(classifier, ("savedClassifiers/" + nameForThisClassifier))
//   	}



// 	opts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))


// 	trainOpts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))



//     println("done.")





//     // Evaluate
//     println("Evaluating...")





//     var evalTakeXLines = 0
//     var evalSliceMethod = "recent"

//     val evalTakeOptList = evalOpts.toList.filter(opt=>opt.contains("take"))

//     if (evalTakeOptList.length == 1) {
//     	val evalTakeOpt = evalTakeOptList(0)
//     	//println("evalTakeOpt should be 1: " + evalTakeOpt)
//     	evalOpts = evalOpts - evalTakeOpt
//     	evalTakeXLines = evalTakeOpt.split(":")(1).toInt
//     	evalSliceMethod = evalTakeOpt.split(":")(2)
//     }


//     val evalExamples = 
//     	if (evalOpts("orig")) {
//     		evalOpts = evalOpts - "orig"
//     		fromListOfFileNamesNoRetweets(evalFile, evalTakeXLines, evalSliceMethod).toList
//     	} else if (evalOpts("articles")) {
//     		labeledArticles().toList
//     	} else {
//     		fromListOfFileNames(evalFile, evalTakeXLines, evalSliceMethod).toList
//     	}



// 	evalOpts.toList.foreach(specification => println("specification unexpected and ignored: " + specification))


//     val comparisons1 = for (ex <- evalExamples) yield
//       (ex.label, classifier.predict(ex.features), ex.features)
//     val (goldLabels, predictions, inputs) = comparisons1.unzip3
//     println(ConfusionMatrix(goldLabels, predictions, inputs))





//     val comparisons2 = for (ex <- evalExamples) yield
//     	(ex.label, ex.id, classifier.labels.zip(classifier.evalRaw(ex.features)), ex.features)
//     comparisons2.foreach{comparison => 
//     	val tuplOfArrays = comparison._3
//     	val id = comparison._2
//     	val goldLabel = comparison._1
//     	println("\n" + id + " " + goldLabel)
//     	tuplOfArrays.foreach{tupl => 
//     		println(tupl._1 + " " + tupl._2)
//     	}
//     }


//   }
  
// }




//     val comparisons = for (ex <- evalExamples) yield
//     	(ex.label, ex.id, classifier.labels.zip(classifier.evalRaw(ex.features)), ex.features)
//     comparisons.foreach{comparison => 
//     	val tuplOfArrays = comparison._3
//     	val id = comparison._2
//     	val goldLabel = comparison._1
//     	println("\n" + id + " " + goldLabel)
//     	tuplOfArrays.foreach{tupl => 
//     		println(tupl._1 + " " + tupl._2)
//     	}
//     }


//   }
  
// }











    // val featurizer = {
    // 	if (opts("bi")) {
    // 		opts = opts - "bi"
    // 		new BigramFeaturizer(stopwords, queryWords, count, liwc, hashtag, false, false, uni)
    // 	} else {
    // 		if (opts("uni")) {opts = opts - "uni"}
    // 		new UnigramFeaturizer(stopwords, queryWords, count, liwc, hashtag, false, false)
    // 	}	
    // }

	// uni: Boolean,
	// uniCount: Boolean,
	// uniStem: Boolean,
	// bi: Boolean,
	// biCount: Boolean,
	// biStem: Boolean,
	// liwc: Boolean,
	// liwcCount: Boolean,
	// hashtags: Boolean,
	// hashtagsCount: Boolean


