package thesiscode.util

import java.io.{File,FileWriter,PrintWriter}
import scala.io.Source
import thesiscode.util.Util._
import scala.collection.mutable.Set
import thesiscode.classify.ClassifyUtil.queryWords

object LiwcCategories {

	val rawLines: Iterator[String] = Source.fromFile("src/main/resources/LIWC2007_English100131.dic").getLines
	rawLines.next
	val categoryLines: Vector[String] = rawLines.toVector.take(64).toVector
	val numCategories = categoryLines.length


	val (catsAsNums, catsAsNames) = categoryLines.map{ line =>
			val numWordArray: Array[String] = line.split("\\s+")
			(numWordArray(0), numWordArray(1))
		}.unzip



	val name_num_tuples = catsAsNames.zip(catsAsNums)
	val name2num: Map[String, String] = name_num_tuples.toMap

	val num_name_tuples = catsAsNums.zip(catsAsNames)
	val num2name: Map[String, String] = num_name_tuples.toMap

	val num_index_tuples = catsAsNums.zipWithIndex
	val num2index: Map[String, Int] = num_index_tuples.toMap

	val indexes = num_index_tuples.indices
	val index_num_tuples = indexes.zip(num_index_tuples.map(x=>x._1))
	val index2num: Map[Int, String] = index_num_tuples.toMap

	val vectorOfWordSets: Vector[scala.collection.mutable.Set[String]] = Vector.fill(numCategories)(Set[String]())

	val word_category_vector: Vector[String] = Source.fromFile("src/main/resources/LIWC2007_English100131.dic").getLines.toVector.drop(66)


	word_category_vector.foreach{line =>
		val split = line.split("\\s+")
		val word = split(0)
		val nums = split.drop(1)
		nums.foreach{ num =>
			vectorOfWordSets(num2index(num)) += word
		}
	}

	val wordSets: Vector[scala.collection.immutable.Set[String]] = 
			vectorOfWordSets.map(s => s.toSet)

	def checkForDictionaryWord (dictionaryWord: String, textWords: Vector[(String,Int)]): Vector[(String,Int)] = {

		def normalCheck (dictionaryWord: String, textWords: Vector[(String,Int)]): Vector[(String,Int)] = {
			textWords.filter(_._1 == dictionaryWord)
		}

		def asteriskCheck (dictionaryWord: String, textWords: Vector[(String,Int)]): Vector[(String,Int)] = {
			textWords.filter(_._1.startsWith(dictionaryWord.dropRight(1)))
		}

		if (dictionaryWord.endsWith("*")) {
			asteriskCheck(dictionaryWord, textWords)
		} else {
			normalCheck(dictionaryWord, textWords)
		}

	}



	def apply(text: String)  = {

		val cleanedText0 = cleanText(text, false, true, true, true)
		val cleanedText = cleanedText0.split("\n").map(line => line + ".").mkString("\n")
		//println(cleanedText)
		val words = cleanedText.split("\\s+").toVector
		//val categoryCounts = Vector.fill(numCategories)(scala.collection.mutable.Map[String, Int]().withDefault(x=>0))
		val wordsWithIndex = words.zipWithIndex
		var wordsInSomeCategory = List[(String, Int)]()

		val vectorOfRawCategoryCounts: Vector[Int] = 
			wordSets.map{ set =>
				set.toVector.map{ dictionaryWord =>
					val matches = checkForDictionaryWord(dictionaryWord, wordsWithIndex)
					matches.foreach(matchTuple => {wordsInSomeCategory = matchTuple :: wordsInSomeCategory})
					matches.filterNot(matchTuple => queryWords(matchTuple._1)).length
				}.sum
			}

		val totalWordCount = words.length
		val moreThan6LettersCount = words.filter(x=> x.length > 6).length
		val totalDictWordsCount = {
				val uniq: scala.collection.immutable.Set[(String, Int)] = wordsInSomeCategory.toSet
				//uniq.foreach(println)
				uniq.size
			}
		val wordsPerSentence = {
			val numSentences = cleanedText.split("""(\.+|;+|…+|:+)+""")
				.map(sent => sent.trim)
				.toVector
				.map(sent => sent.split("\\s+"))
				.filterNot(sentence => sentence.length == 0)
				.length
			totalWordCount.toDouble/numSentences.toDouble
		}
		val vectorOfProportions = vectorOfRawCategoryCounts.map(count => count.toDouble/totalWordCount).toList

		val extraFeatures = List(totalWordCount, wordsPerSentence, totalDictWordsCount, moreThan6LettersCount)

		val featureValues = (extraFeatures ++ vectorOfProportions).toVector

		val featureKeys = (List("wc", "wps", "dic", "sixltr") ++ (catsAsNames.toList)).toVector

		val features = featureKeys.zip(featureValues)

		// print("sexual: ")
		// println(features(num2index(name2num("sexual")) + 4))

		features

	}

	// def main(args: Array[String]): Unit = {
	// 	println("Hello!")
	// }






}
