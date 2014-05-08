package thesiscode.dataprocessing

import scala.io.Source


/** These classes are for serializing the list of users and labels (LUL) file
**/


class SubtopicLabel ( //e.g. "god", e.g. "spirit.mid"
	val fullSubtopicString: String,
	superTopicRate: String
	) {
	val parts: Array[String] = {
		if (fullSubtopicString.contains(".")==true) { fullSubtopicString.split("\\.")
		} else { Array(fullSubtopicString)
		}
	}
	val subtopic = parts(0)
	val rate = {
		if (parts.length == 1) {
			superTopicRate
		} else {
			parts(1)
		}
	}
}

class TopicLabel ( //relig.ext:god;spirit.mid
	val fullTopicString: String
	) {
	val parts = fullTopicString.split(":")
	val fullTopic = parts(0)
	val fullSubtopicString = {
		if (parts.length==2) { parts(1)
		} else {"none.none"}
	}
	// val Array(fullTopic, fullSubtopicString) = fullTopicString.split(":")
	//   relig.ext  god;spirit.mid
	val Array(topic, rate) = fullTopic.split("\\.")
	val subtopicStringArray = fullSubtopicString.split(";|,") //["god","spirit.mid"]
	val subtopicLabels = subtopicStringArray.map(new SubtopicLabel(_, rate))
	val numberOfSubtopics = {
		if (fullSubtopicString == "none.none") { 0
		} else {subtopicStringArray.length}
	}
}

class PerspectiveLabel ( //ps.adv.ext
	val fullPerspectiveString: String
	) {
	val Array(perspective, level, rate) = fullPerspectiveString.split("\\.")
}


class Line (
	line: String //e.g. "NopeNotThisTime sc.adv.ext-relig.ext:god-science.mid:evol"
	) {
	val Array(name, fullString) = line.split(" ").slice(0,2)
	//NopeNotThisTime, sc.adv.ext-relig.ext:god-science.mid:evol
	val label = new Label(fullString)
	// val parts = fullString.split("-") //[sc.adv.ext,relig.ext:god,science.mid:evol]
	// val positionString = parts(0) //sc.adv.ext
	// val positionLabel = new PositionLabel(positionString)
	// val topicStringArray = parts.drop(1) //[relig.ext:god,science.mid:evol]
	// val numberOfTopics = topicStringArray.length
	// val topicLabels = topicStringArray.map(x => new TopicLabel(x))
	
}


class Label (
	val string: String //e.g. "sc.adv.ext-relig.ext:god-science.mid:evol"
	) {
	val parts = string.split("-") //[sc.adv.ext,relig.ext:god,science.mid:evol]
	val perspectiveString = parts(0) //sc.adv.ext
	val perspectiveLabel = new thesiscode.dataprocessing.PerspectiveLabel(perspectiveString)
	val topicStringArray = parts.drop(1) //[relig.ext:god,science.mid:evol]
	val numberOfTopics = topicStringArray.length
	val topicLabels = topicStringArray.map(x => new thesiscode.dataprocessing.TopicLabel(x))
}

object Reader {

	import twitter4j._

	// Turn List of Users and Labels (LUL) file into a CSV output (for putting into R)

	var listOfLabelCSVstrings = List[String]()
	
	def mkCSVlabelsFromUser (l: Line): Unit = {

		val topicLabels = l.label.topicLabels
		topicLabels.foreach( x =>
			listOfLabelCSVstrings = List(l.name, l.label.perspectiveLabel.perspective, l.label.perspectiveLabel.level, l.label.perspectiveLabel.rate, x.topic, x.rate, x.subtopicLabels.map(x=>x.subtopic).mkString("/"), x.subtopicLabels.map(x=>x.rate).mkString("/")).mkString(",") :: listOfLabelCSVstrings)
		
	}

	//((bob,sc),(topic1,topic2))

	def printPerLabelCSV (users: Vector[Line]):Unit = {
		val firstLine = "Name,PLabel,PLevel,PRate,TLabel,TRate,STLabels,STRates"
		users.map(mkCSVlabelsFromUser)
		listOfLabelCSVstrings = firstLine :: listOfLabelCSVstrings
		listOfLabelCSVstrings.foreach(println)


	}

	val twitter = new TwitterFactory().getInstance()

	def userExists(username: String): Unit = {
	    try { 
	    	println(twitter.showUser(username).getScreenName)
	        } catch {
	           case te: TwitterException => {
	           	println(username + " DOES NOT EXIST ----------------------")
	           }
	        }
	}

	def main (args: Array[String]) = {

		val lines = Source.fromFile("data/twitterUsers/mainList.txt").getLines.toVector
		val filtered = lines.filter(x=>(x.startsWith("\"")==false)&&(x.contains("*confl")==false))
		val userNames = filtered.map{x =>
			//println(x)
			val l = new Line(x)
			val n = l.name
			val p = l.label.perspectiveLabel.perspective
			val tl = l.label.topicLabels
			val t = tl.map(_.topic).mkString(",")
			(n + "\t\t\t\t" + p + "\t\t\t\t" + t)
			}

	}

}







/** This code will probably not prove useful for anyone other than me. It takes 
a text file with lines of the form "Twitter_username label(s)" 
(e.g. "NopeNotThisTime sc.adv.ext-relig.ext:god-science.mid:evol") and produces 
an unordered list of every "morpheme" of the labeling system, along with its 
frequency overall in the text file. **/

object LabelLister {
	
	import scala.io.Source

	def main (args: Array[String]) {
		val lines = Source.fromFile(args(0)).getLines.toVector
		//"NopeNotThisTime sc.adv.ext-relig.ext:god-science.mid:evol"
		val filtered = lines.filter(x=>(x.startsWith("\"")==false&&x.contains("*confl")==false))
		val splits = filtered.map(x=>x.split(" "))
		val fullLabels = splits.map(x=>x(1))
		val vectorOfLabelFragmentArrays = fullLabels.map(x=>x.split("""[:,\.;-]""").toVector)
		val vectorOfLabelFragments = vectorOfLabelFragmentArrays.flatten
		val counts = vectorOfLabelFragments.groupBy(x=>x).mapValues(x=>x.length)

		(for (key <- counts.keys) yield (key + " " + counts(key))).foreach(println)


	}
}

//for checking the labeled users list

/** I am certain this code will not be useful except for me, and for a very brief time. 
It is for making sure the LUL (list of users and labels) file is generally in the
right format **/

object BasicLabelFormatChecker {
	
	import scala.io.Source

// checks that the format is "blahblah (ps|sc)blahblah"

	val BasicLineRE = """\w+ (?:ps|sc).+""".r

	def printBasic (line: String) =  {
		if (BasicLineRE.pattern.matcher(line).matches) { println("match")
		} else { println("FAILURE TO MATCH: " + line)}
	}

	def basicCheck (filename: String) {
		val lines = Source.fromFile(filename).getLines.toVector
		//"NopeNotThisTime sc.adv.ext-relig.ext:god-science.mid:evol"
		val filtered = lines.filter(x=>(x.contains("\"")==false))
		filtered.foreach(x=>printBasic(x))
	}

		def main (args: Array[String]) {
			basicCheck(args(0))
	}
}






//Tells you what lines don't fit the desired format as defined in LineRE below.

// object LabelChecker {
	
// 	import scala.io.Source

// 	def main (args: Array[String]) {
// 		val name = """\w+"""
// 		val view = """(?:sc|ps)"""
// 		val level = """(?:adv|hiadv|hi|midhi|mid|midlo|lo|lobar|bar)"""
// 		val rate = """(?:ext|hiext|hi|midhi|mid|midlo|lo|lorar|rar)"""
// 		val label = name
// 		val firstPart = name + " " + view + """\.""" + level + """\.""" + rate
// 		val secondPart = """(-""" + label + """\.""" + rate + """(:""" + label + """(\."""+ rate + """)?(;""" + label + """(\.""" + rate + """)?)*)*)+"""
// 		val LineRE = ("""(""" + firstPart + secondPart + """)""").r
// 		// val LineRE = """\w+ (?:ps|sc)\.\w+\.\w+(?:-\w+\.\w+(?::\w+(?:.\w+)?(?:,\w+(?:.\w+)?)*)+)+""".r
// 		// val LineRE = \w+ (?:ps|sc)\.\w+\.\w+    (-health.hi    (:vax(.mid)?(,vax(.mid)?)) )+
// 		val lines = Source.fromFile(filename).getLines.toVector
// 		//"NopeNotThisTime sc.adv.ext-relig.ext:god-science.mid:evol"
// 		val filtered = lines.filter(x=>(x.contains("\"")==false))
// 		val nonMatches = filtered.map(x => x match {case i })
// 	}
	


// 	}
// }












// object Summary {

// 		def main (args: Array[String]) = {

// 		val lines = Source.fromFile("data/twitterUsers/goodList.txt").getLines.toVector
// 		val filtered = lines.filter(x=>(x.startsWith("\"")==false)&&(x.contains("*confl")==false))
// 		val users = filtered.map(x =>
// 			println((new UserLabel(x)).name)
// 			)



// 	}

// }


//"NopeNotThisTime sc.adv.ext-relig.ext:god-science.mid:evol"