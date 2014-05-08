package thesiscode.twitter

import thesiscode.dataprocessing._
import scala.io.Source
import java.io.File
import thesiscode.util.Util._
import twitter4j._
import collection.JavaConversions._
import play.api.libs.json.Json



/////////////////////////////
/////////////////////////////

object Configs {
	val config = new twitter4j.conf.ConfigurationBuilder()  //jj_anlp     tweetcollector 1 or 2
    .setOAuthConsumerKey("y8pUXOlTCy0LMDZJDwaE3g")
    .setOAuthConsumerSecret("3wra3dYNhOdcotfaIWRbm3ag5Ra0clR8qonVSqjs")
    .setOAuthAccessToken("1407632820-F9ieuRiExjKECwaVQKWAOZVfR60B79JnDA1XrTJ")
    .setOAuthAccessTokenSecret("Rmru6E0EqVMYQoqF2xsXY4nthHlVqhuFTRPQuM1XgZo")
    .setJSONStoreEnabled(true)
    .build


	val config2 = new twitter4j.conf.ConfigurationBuilder()  //jj_anlp     tweetcollector 1 or 2
    .setOAuthConsumerKey("ISodfKtgjeZ76vcLpuROg")
    .setOAuthConsumerSecret("mt7clm9xihJy1F7EhJIZp8HOOrKCp9hZKbFDDYrJ4")
    .setOAuthAccessToken("1123605330-Yh1tX1VzkxfYLVaEF6pE8PiPjfE2C06z1FlT8PT")
    .setOAuthAccessTokenSecret("j2GzIQjNoIOm7AQqpJhEjjTwq3jlH4anEmfugmnQ")
    .setJSONStoreEnabled(true)
    .build
}



/////////////////////////////
/////////////////////////////



class CorpusCollectorJSON (
	val listName: String,
	val saveConfl: Boolean = true
	) {

	val twitter = new TwitterFactory().getInstance()
	val pathToList = "data/twitterUsers/"
	val lines = scala.io.Source.fromFile(pathToList + listName).getLines
	val filtered = if (saveConfl == true) {
			lines.filter(x=>(x.startsWith("\"")==false))
		} else { 
			lines.filter(x=>(x.startsWith("\"")==false)&&(x.contains("*confl")==false))
		}


	def getTweets(line: String) = {

		val split = line.split("\\s+")
		val name = split(0)
		val label = split.drop(1).mkString(" ")
		val t4jUser = twitter.showUser(name)
		val userStatusesCount = t4jUser.getStatusesCount
		val statusesCount = {
				if (userStatusesCount < 1000) {
					userStatusesCount
				} else {
					1000
				}
			}
		var page = new Paging(1,200)
		var tweetsResponseList = twitter.getUserTimeline(name, page)
		var tweetsList = tweetsResponseList.iterator.toList

		var jsonList = tweetsList.map(status => twitter4j.json.DataObjectFactory.getRawJSON(status))
		var fullJsonList = jsonList.reverse
		var lengthOfCurrentList = jsonList.length
		var numOfTweetsSoFar = lengthOfCurrentList
		var maxIdForNewPaging:Long = 1L

		var RLstatus_RL = tweetsResponseList.getRateLimitStatus
		var rem_RL = RLstatus_RL.getRemaining()
		if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 5000)}

		var RLstatus_U = t4jUser.getRateLimitStatus
		var rem_U = RLstatus_U.getRemaining()
		if (rem_U == 0) {Thread.sleep(RLstatus_U.getSecondsUntilReset() * 1000 + 5000)}

		if (tweetsList.isEmpty) {
			println(name + " ---- NO TWEETS")
			throw new TwitterException("No Tweets")
		} else {
			println(name)
		}

		
		while (numOfTweetsSoFar < statusesCount && lengthOfCurrentList != 0) {
			maxIdForNewPaging = tweetsList.map(status => status.getId).sorted.head-1L
			page = (new Paging(1,200)).maxId(maxIdForNewPaging)
			tweetsResponseList = twitter.getUserTimeline(name, page)
			tweetsList = tweetsResponseList.iterator.toList
			jsonList = tweetsList.map(status => twitter4j.json.DataObjectFactory.getRawJSON(status))
			lengthOfCurrentList = jsonList.length

			jsonList.foreach{ tweet =>
				fullJsonList = tweet :: fullJsonList
			}

			numOfTweetsSoFar += lengthOfCurrentList

			RLstatus_RL = tweetsResponseList.getRateLimitStatus
			rem_RL = RLstatus_RL.getRemaining()
			if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 5000)}

			RLstatus_U = t4jUser.getRateLimitStatus
			rem_U = RLstatus_U.getRemaining()
			if (rem_U == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 5000)}
		}

		// // prints all tweets to terminal

		// println("\n------- the tweets of " + name + " ---------")
		// fullJsonList.foreach(x => println(userLabel.fullString + x))

		// prints all tweets to files by user

		val corpusJsonPath = "data/corpusJSON/"

		val dirPath = corpusJsonPath + listName.dropRight(4) + "/"

		val normalDir = new File(dirPath + "main/")
		if (normalDir.exists == false) normalDir.mkdirs

		val conflDir = new File(dirPath + "confl/")
		if (saveConfl == true && conflDir.exists == false) conflDir.mkdirs


		val pathString = if (saveConfl == true) {
				if (label.contains("confl")) {
					dirPath + conflDir.getName + "/" + name + ".txt"
				} else {
					dirPath + normalDir.getName + "/" + name + ".txt"
				}
			} else {
				dirPath + normalDir.getName + "/" + name + ".txt"
			}

		printToFile(new File(pathString))(p => {
			p.println(label)
			fullJsonList.foreach(jsonTweet => p.println(jsonTweet))
	 		})
	}


	def tryGetTweets (line: String) = {
		try { getTweets(line)
	        } catch {
	           case te: TwitterException => {
	           	println("------- TWITTER4J EXCEPTION: " + line.split(" ")(0) + " ---------")
	           	println(te.getErrorMessage)
	           }
	           case npe: java.lang.NullPointerException => println("------- NULL POINTER: " + line.split(" ")(0) + " ---------")
	        }
	}

	def makeCorpus (): Unit  = {
		filtered.foreach(tryGetTweets)
	}	


}// JSONcollector



/////////////////////////////
/////////////////////////////


object CorpusCollectorJSON {
	def main(args: Array[String]) {
		new CorpusCollectorJSON(args(0), args(1).toBoolean).makeCorpus
	}
}

/////////////////////////////
/////////////////////////////



object MemberCollector {

	import twitter4j._
	import scala.io.Source
	import java.io._
	import collection.JavaConversions._

	val lines = scala.io.Source.fromFile("data/listOfTwitterLists2.txt").getLines
	val filtered = lines.filterNot(x=>x.startsWith("\""))
		//e.g. """https://twitter.com/rowenapavlou/lists/☆-spiritual-exploration-☆ ps.adv.hi-relig.mid:spirit-self.hi"""
	val RE = """^https://twitter\.com/(\w+)/lists/(\S+)\s+(.+\S)""".r

	val file = new File("data/twitterUsers/listBasedList2.txt")

	val twitter = new TwitterFactory().getInstance()
	var motherListOfMembers = List[String]()
	var seenMembers = Set[String]()


	def getMembers (l: String): Unit = {
		val line = {
			val split = l.split("\\s+")
			val opt = split.last
			if (opt.replaceAll("""\d""", "") == "") {
				split.dropRight(1).mkString(" ")
			} else {
				l
			}
		}
		val RE(screenName, slug, label) = line
		var listOfMembers = List[String]()
		var cursor: Long = -1
		while (cursor != 0 && listOfMembers.length < 200) {
			val pageOfMembers: PagableResponseList[User] = twitter.getUserListMembers(screenName, slug, cursor)
			val members: List[User] = pageOfMembers.iterator.toList.reverse
			members.foreach { member => 
				if (seenMembers(member.getScreenName)==false) {
				//println(member.getScreenName)
				listOfMembers = (member.getScreenName +" " + label).trim :: listOfMembers
				seenMembers =  seenMembers + member.getScreenName
				}
			}
			cursor = pageOfMembers.getNextCursor
			if (pageOfMembers.getRateLimitStatus.getRemaining() == 0)
				Thread.sleep(pageOfMembers.getRateLimitStatus.getSecondsUntilReset() * 1000 + 5000)
		}

		println(label + " " + listOfMembers.length + " " + slug)
		motherListOfMembers = listOfMembers ++ motherListOfMembers


	}



	def tryGetMembers (line: String) = {
		try { getMembers(line)
	        } catch {
	           case te: TwitterException => {
	           	println("------- TWITTER4J EXCEPTION: " + line.split(" ")(0) + " ---------")
	           	println(te.getErrorMessage)
	           }
	           case npe: java.lang.NullPointerException => println("------- NULL POINTER: " + line.split(" ")(0) + " ---------")
	        }
	}



	def main(args: Array[String]):Unit = {

		filtered.foreach(tryGetMembers)
		printToFile(file)(p => {
			motherListOfMembers.foreach(line => p.println(line))
			})
	}
}


/////////////////////////////
/////////////////////////////




object Spritzer {

	val file = new java.io.File("data/corpusJSON/just-a-test.txt")
	val pw = new java.io.PrintWriter(file)
	var counter: Int = 0
  
	def fileWritingStatusListener = new StatusListener() {
		def onStatus(status: Status) { 
			//if (counter == 1000000) throw new java.lang.Exception("Reached 1000000")
			val jsonString = twitter4j.json.DataObjectFactory.getRawJSON(status)
			//val json = play.api.libs.json.Json.parse(jsonString)
			val lang = status.getLang//(json \ "lang").asOpt[String]
			//if (lang != "") {
			if (lang == "en") {
				pw.println(jsonString)
				pw.flush()
				//counter += 1
			}
			//}//if
		}
		def onDeletionNotice(statusDeletionNotice: StatusDeletionNotice) {}
		def onTrackLimitationNotice(numberOfLimitedStatuses: Int) {}
		def onException(ex: Exception) { ex.printStackTrace }
		def onScrubGeo(arg0: Long, arg1: Long) {}
		def onStallWarning(warning: StallWarning) {}
	}

	def main(args: Array[String]) {
	    val twitterStream = new TwitterStreamFactory().getInstance
	    twitterStream.addListener(fileWritingStatusListener)
	    twitterStream.sample
	    Thread.sleep(java.util.concurrent.TimeUnit.SECONDS.toMillis(5))
	    twitterStream.cleanUp
	    twitterStream.shutdown
	}
}






/////////////////////////////
/////////////////////////////


trait Twitter4jCollector {
	import twitter4j._


	def tryGetOrigUserId(status:Status):String = {
	    try { status.getRetweetedStatus.getUser.getId.toString
	        } catch {
	           case t: Throwable => ""
	        }
	}

	def tryGetOrigTweetId(status:Status):String = {
	    try { status.getRetweetedStatus.getId.toString
	        } catch {
	           case t: Throwable => ""
	        }
	}

	def shrinkDate(dateString: String):String = {    //"Sat Jul 27 14:34:11 CDT 2013"
	  										// becomes "072713CDT143411"                                                  
	    val splitUp = dateString.split(" ")
	    val Array(_, month, day, hour, zone, year) = splitUp
	    val monthDict = Vector("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	        .zip(Vector("01","02","03","04","05","06","07","08","09","10","11","12"))
	        .toMap
	    monthDict(month) + day + year.drop(2) + zone + hour.split(":").mkString
	}


	def booleanToLetter(bool:Boolean): String = {
	    val str = bool.toString
	    str(0).toString
	}



	def tryGetOrigTweetText(status:Status):String = {
	    try { 
	    	status.getRetweetedStatus.getText
	    } catch {
	        case t: Throwable => status.getText
	    }
	}

	def tryGetOrigTweetURLs(status:Status):String = {


		val urls = {
		    try { 
		    	status.getRetweetedStatus.getURLEntities.map(ue => ue.getURL)
		    } catch {
		        case t: Throwable => status.getURLEntities.map(ue => ue.getURL)
		    }
		}

		val media = {
		    try { 
		    	status.getRetweetedStatus.getMediaEntities.map(me => me.getURL)
		    } catch {
		        case t: Throwable => status.getMediaEntities.map(me => me.getURL)
		    }
		}

		val all = urls ++ media
		all.mkString("\"")
	}


	val WhitespaceRE = """\s+""".r

	//text, entities, original author and date

	def getTweetInfo (status: Status) = {

		val rt = status.isRetweet

		val text = if (rt) {
				tryGetOrigTweetText(status)
			} else {
				status.getText
			}

		val urls = if (rt) {
				tryGetOrigTweetURLs(status)
			} else {
				status.getURLEntities.map(ue => ue.getURL).mkString("\"")
			}

		Vector(
	        status.getUser.getId,
	        shrinkDate(status.getCreatedAt.toString),
	        status.getId,
	        booleanToLetter(status.isRetweet),
	        status.getRetweetCount,
	        tryGetOrigTweetId(status),
	        tryGetOrigUserId(status),
	        status.getInReplyToUserId,
	        status.getInReplyToStatusId,
	        urls,
	        WhitespaceRE.replaceAllIn(text, " ")
	        ).mkString(",")
		//1123605330
		//030513CST134825
		//309027638515732480
		//f
		//0
		//0
		//0
		//-1
		//-1
		//test
	}
}//trait




/////////////////////////////
/////////////////////////////




object CorpusCollectorT4J extends Twitter4jCollector {

	import twitter4j._
	import scala.io.Source
	import java.io._
	import collection.JavaConversions._
	import thesiscode.util.Util


	val lines = scala.io.Source.fromFile("data/twitterUsers/mainList.txt").getLines
	val filtered = lines.filter(x=>(x.startsWith("\"")==false)&&(x.contains("*confl")==false))
	//val namesLabels = for (line <- filtered) yield line.split(" ").slice(0,2).mkString(" ")
	
	val twitter = new TwitterFactory().getInstance()

	def getTweets(line: String) = {

		val split = line.split("\\s+")
		val name = split(0)
		val label = split.drop(1).mkString(" ")

		// val line = new Line(line)
		// val name = line.name
		// val position = line.perspectiveLabel.perspective

		val t4jUser = twitter.showUser(name)
		val userStatusesCount = t4jUser.getStatusesCount
		val statusesCount = {
			if (userStatusesCount < 1000) {
				userStatusesCount
			} else {
				1000
			}
		}

		var page = new Paging(1,200)
		var tweetsResponseList = twitter.getUserTimeline(name, page)
		var tweetsList = tweetsResponseList.iterator.toList
		var fullList = tweetsList.reverse
		//fullList.foreach(t=>println("\n" + t.getText + "\n"))
		var lengthOfCurrentTweetsList = tweetsList.length
		if (tweetsList.isEmpty) {
			println(name + " ---- NO TWEETS")
			throw new TwitterException("No Tweets")
			} else {
			println(name)
		}
		var numOfTweetsSoFar = lengthOfCurrentTweetsList
		var maxIdForNewPaging:Long = 1L

		var RLstatus_RL = tweetsResponseList.getRateLimitStatus
		var rem_RL = RLstatus_RL.getRemaining()
		if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}

		var RLstatus_U = t4jUser.getRateLimitStatus
		var rem_U = RLstatus_U.getRemaining()
		if (rem_U == 0) {Thread.sleep(RLstatus_U.getSecondsUntilReset() * 1000 + 1000)}

		
		while (numOfTweetsSoFar < statusesCount && lengthOfCurrentTweetsList != 0) {
			maxIdForNewPaging = tweetsList.map(status => status.getId).sorted.head-1L
			page = (new Paging(1,200)).maxId(maxIdForNewPaging)
			tweetsResponseList = twitter.getUserTimeline(name, page)
			tweetsList = tweetsResponseList.iterator.toList
			lengthOfCurrentTweetsList = tweetsList.length
			//tweetsList.reverse.foreach(tweet=>println(tweet.getText))

			tweetsList.foreach{ tweet =>
				fullList = tweet :: fullList
			}

			numOfTweetsSoFar += lengthOfCurrentTweetsList

			RLstatus_RL = tweetsResponseList.getRateLimitStatus
			rem_RL = RLstatus_RL.getRemaining()
			if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}

			RLstatus_U = t4jUser.getRateLimitStatus
			rem_U = RLstatus_U.getRemaining()
			if (rem_U == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}
		}

		// // prints all tweets to terminal

		// println("\n------- the tweets of " + name + " ---------")
		// println(userLabel.fullString)
		// fullList.foreach(tweet => println(getTweetInfo(tweet)))

		// prints all tweets to files by user
		printToFile(new File("data/corpus/byUser2/" + name + ".txt"))(p => {
				p.println(label)
	 			fullList.foreach(tweet => p.println(getTweetInfo(tweet)))
	 		})

		// // creates a directory to put individual tweet-files in
		// val prefix = "data/corpus/individually2/" + name + "/"

		// val userDir = (new File(prefix).mkdir())

		// // print each tweet to its own file, putting in the directory created above.
		// val labelDir = new File(prefix + "label").mkdir()

		// printToFile(new File(prefix + "label/label.txt"))(p => {
	 // 		p.println(userLabel.fullString)
	 // 	})

		// for (tweet <- fullList) {
		// 	printToFile(new File(prefix + tweet.getId + ".txt"))(p => {
	 // 			p.println(getTweetInfo(tweet))
	 // 		})
		// }
	

	}//getTweets


	def tryGetTweets (line: String) = {
		try { getTweets(line)
	        } catch {
	           case te: TwitterException => println("\n------- TWITTER4J EXCEPTION: " + line.split(" ")(0) + " ---------")
	           case npe: java.lang.NullPointerException => println("\n------- NULL POINTER: " + line.split(" ")(0) + " ---------")
	        }
	}

	

	def main(args: Array[String]):Unit = {
		//println("works")
		//List("evans_anlp ps.hi.hi-health.mid:herb.mid-relig.mid:sup.mid", "dave1990brown ps.hi.hi-health.mid:herb.mid-relig.mid:sup.mid").foreach(tryGetTweets(_))
		getTweets("evans_anlp ps.hi.hi-health.mid:herb.mid-relig.mid:sup.mid")
		//namesLabels.foreach(tryGetTweets(_))
		//getTweets("GSpellchecker sc.adv.ext-relig.ext:god")


	}
}



/////////////////////////////
/////////////////////////////



























































































	// get subscribers

	// var listOfSubscribers = List[String]()

	// var cursor2: Long = -1
	// while (cursor2 != 0) {
	// 	val pageOfSubscribers = twitter.getUserListSubscribers(listId, cursor2)
	// 	val subscribers = pageOfSubscribers.toArray.map(_.getScreenName)
	// 	subscribers.foreach { name => 
	// 		println(name)
	// 		listOfTuples = name :: listOfSubscribers
	// 	}
	// 	cursor2 = pageOfSubscribers.getNextCursor
	// 	if (pageOfSubscribers.getRateLimitStatus.getRemaining() == 0)
	// 		Thread.sleep(pageOfSubscribers.getRateLimitStatus.getSecondsUntilReset() * 1000 + 5000)
	// }

	// def getTweets (line: String) = {


	// }

	// val goalNumber = 1000
	// var page = new Paging(1,200)
	// var tweetsResponseList = twitter.getUserListStatuses(listId, page)
	// var tweetsList = tweetsResponseList.iterator.toList
	// var fullTweetsList = tweetsList
	// var lengthOfCurrentTweetsList = tweetsList.length
	// var numOfTweetsSoFar = lengthOfCurrentTweetsList
	// var maxIdForNewPaging:Long = 1.toLong

	// 	var RLstatus_RL = tweetsResponseList.getRateLimitStatus
	// 	var rem_RL = RLstatus_RL.getRemaining()
	// 	if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}

		
	// 	while (lengthOfCurrentTweetsList == 200 && numOfTweetsSoFar < goalNumber ) {
	// 		maxIdForNewPaging = tweetsList.map(status => status.getId).sorted.head
	// 		page = (new Paging(1,200)).maxId(maxIdForNewPaging)
	// 		tweetsResponseList = twitter.getUserTimeline(listId, page)
	// 		tweetsList = tweetsResponseList.iterator.toList
	// 		lengthOfCurrentTweetsList = tweetsList.length

	// 		tweetsList.foreach{ tweet =>
	// 			fullTweetsList = tweet :: fullTweetsList
	// 		}

	// 		numOfTweetsSoFar = fullTweetsList.length

	// 		RLstatus_RL = tweetsResponseList.getRateLimitStatus
	// 		rem_RL = RLstatus_RL.getRemaining()
	// 		if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}

	// 	}

	// // prints all tweets to terminal

	// 	println("\n------- the tweets of " + slug + " ---------")
	// 	fullTweetsList.foreach(x => println(x))

	// // prints all tweets to files by List
	// 	printToFile(new File("data/corpus/byList/" + slug + ".txt"))(p => {
	//  			fullTweetsList.foreach(x=> p.println(x))
	//  		})




	// // getUserListMembers(listid, cursor)
	// // getUserListSubscribers(listid, cursor)





























/////////////////////////////

// object 


// val prefix = "data/twitterUsers/smallFiles"
// val LUL = Source.fromFile("data/twitterUsers/list.txt").getLines
// val filteredLUL = lines.filter(x=>x.startsWith("\"")==false)







//////////////////////////////

// trait Collector {
	
// 	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
// 		  val p = new java.io.PrintWriter(f)
// 		  try { op(p) } finally { p.close() }
// 	}

// }
// // used like this:
// // import java.io._
// // val data = Array("Five","strings","in","a","file!")
// //
// // printToFile(new File("example.txt"))(p => {
// //   data.foreach(p.println)
// // })

// /////////////////////////////
// /////////////////////////////
























































































































// trait CollectorX {
	
// 	import twitter4j._


// 	val twitter = new TwitterFactory().getInstance()


// 	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
// 		  val p = new java.io.PrintWriter(f)
// 		  try { op(p) } finally { p.close() }
// 	}


// 	def shrinkDate(dateString: String):String = {     //"Sat Jul 27 14:34:11 CDT 2013"

//     val splitUp = dateString.split(" ")
//     val Array(_, month, day, hour, zone, year) = splitUp
//     val monthDict = Vector("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
//         .zip(Vector("01","02","03","04","05","06","07","08","09","10","11","12"))
//         .toMap
//     val finalString = monthDict(month) + day + year.drop(2) + zone + hour.split(":").mkString
//     finalString
//   }

// 	// take a Status, username and label
// 	// // remove URLs, @ mentions, hashtags, and "RT : "
// 	// def createLine(s: twitter4j.Status, label: String) = {
//  //  		val text = s.getText()
//  //  		val urls = s.getURLEntities.map(x=>x.getURL)
//  //    	val text2 = text
//  //    		.replaceAll("[@#][A-Za-z0-9_]+ ?","")
//  //      	var text3 = text2.replaceAll("RT : ", "")
//  //      	urls.foreach{ url =>
//  //      		text3 = text3.replaceAll(url,"")
//  //      	}
//  //      	//val metadata = getMetadata(s)
//  //      	//val line = label + " " + metadata + " " + text3
//  //      	(label + " " + text3).replaceAll("\\s+"," ")
//  //      }



// 	// take a Status, username and label
// 	// remove URLs, @ mentions, hashtags, and "RT : "
// 	def createLine(s: twitter4j.Status, label: String) = {

//     	List(
// 	    	label
// 	        shrinkDate(s.getCreatedAt.toString),
// 	        s.getId,
// 	        booleanToLetter(s.isRetweet),
// 	        s.getRetweetCount,
// 	        tryGetOrigTweetId(s),
// 	        tryGetOrigUserId(s),
// 	        s.getInReplyToUserId,
// 	        s.getInReplyToStatusId,
// 	        s.getText.replaceAll("\\s+", " ")
//         ).mkString(",")

//       }


	
// }











// object MainCorpusCollectorX extends Collector {

// 	import twitter4j._
// 	import scala.io.Source
// 	import java.io._
// 	import collection.JavaConversions._


//       }

// 	val lines = scala.io.Source.fromFile("data/twitterUsers/list.txt").getLines
// 	val filtered = lines.filter(x=>x.startsWith("\"")==false)
// 	val namesLabels = for (line <- filtered) yield line.split(" ").slice(0,2).mkString(" ")
	



// 	def getTweets(line: String) = {

// 		val userLabel = new UserLabel(line)
// 		val name = userLabel.name
// 		val position = userLabel.positionLabel.position

// 		val t4jUser = twitter.showUser(userLabel.name)
// 		val statusesCount = t4jUser.getStatusesCount

// 		var page = new Paging(1,200)
// 		var tweetsResponseList = twitter.getUserTimeline(name, page)
// 		var tweetsList = tweetsResponseList.iterator.toList
// 		var fullTweetsList = tweetsList
// 		var lengthOfCurrentTweetsList = tweetsList.length
// 		var numOfTweetsSoFar = lengthOfCurrentTweetsList
// 		var maxIdForNewPaging:Long = 1000000000000000L

// 		var RLstatus_RL = tweetsResponseList.getRateLimitStatus
// 		var rem_RL = RLstatus_RL.getRemaining()
// 		if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}

// 		var RLstatus_U = t4jUser.getRateLimitStatus
// 		var rem_U = RLstatus_U.getRemaining()
// 		if (rem_U == 0) {Thread.sleep(RLstatus_U.getSecondsUntilReset() * 1000 + 1000)}

		
// 		while (numOfTweetsSoFar < statusesCount) {
// 			maxIdForNewPaging = tweetsList.map(status => status.getId).sorted.head
// 			page = (new Paging(1,200)).maxId(maxIdForNewPaging)
// 			tweetsResponseList = twitter.getUserTimeline(name, page)
// 			tweetsList = tweetsResponseList.iterator.toList
// 			lengthOfCurrentTweetsList = tweetsList.length

// 			tweetsList.foreach{ tweet =>
// 				fullTweetsList = tweet :: fullTweetsList
// 			}

// 			numOfTweetsSoFar = fullTweetsList.length

// 			RLstatus_RL = tweetsResponseList.getRateLimitStatus
// 			rem_RL = RLstatus_RL.getRemaining()
// 			if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}

// 			RLstatus_U = t4jUser.getRateLimitStatus
// 			rem_U = RLstatus_U.getRemaining()
// 			if (rem_U == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}
// 		}

// 		// prints all tweets to terminal

// 		println("\n------- the tweets of " + name + " ---------")
// 		fullTweetsList.foreach(x => println(createLine(x, name, userLabel.fullString)))

// 		// prints all tweets to files by user
// 		printToFile(new File("data/corpus/byUser/" + name + ".txt"))(p => {
// 	 			fullTweetsList.foreach(x=> p.println(createLine(x, name, userLabel.fullString)))
// 	 		})

// 		// creates a directory to put individual tweet-files in
// 		val prefix = "data/corpus/individually/" + name + "/"

// 		val userDir = (new File(prefix).mkdir())

// 		// print each tweet to its own file, putting in the directory created above.
// 		// BUT IGNORE RETWEETS

// 		for (tweet <- fullTweetsList) {
// 			printToFile(new File(prefix + tweet.getId + ".txt"))(p => {
// 	 			if (tweet.isRetweet==false) p.println(createLine(tweet, name, userLabel.fullString))
// 	 		})
// 		}
	

// 	}

	

// 	def main(args: Array[String]):Unit = {
// 		//println("works")
// 		getTweets("evans_anlp ps.hi.hi-health.mid:herb.mid-relig.mid:sup.mid")

// 	}
// }







// object ListCollectorX extends Collector {

// 	import twitter4j._
// 	import scala.io.Source
// 	import java.io._
// 	import collection.JavaConversions._

// 	// val theirLists_subscriber = 

// 	// val theirLists_member = twitter.showUser(TheAcronymMaker)

// // get list ID
// 	val slug = "positive-viberz"
// 	val posVib = twitter.showUserList(twitter.showUser("tsuliena").getId, slug)
// 	val listId = posVib.getId

// // get members

// 	var listOfMembers = List[String]()

// 	var cursor: Long = -1
// 	while (cursor != 0) {
// 		val pageOfMembers: PagableResponseList[User] = twitter.getUserListMembers(listId, cursor)
// 		val members: List[User] = pageOfMembers.iterator.toList
// 		members.foreach { member => 
// 			println(member.getScreenName)
// 			listOfMembers = member.getScreenName :: listOfMembers
// 		}
// 		cursor = pageOfMembers.getNextCursor
// 		if (pageOfMembers.getRateLimitStatus.getRemaining() == 0)
// 			Thread.sleep(pageOfMembers.getRateLimitStatus.getSecondsUntilReset() * 1000 + 5000)
// 	}


// // get subscribers

// 	// var listOfSubscribers = List[String]()

// 	// var cursor2: Long = -1
// 	// while (cursor2 != 0) {
// 	// 	val pageOfSubscribers = twitter.getUserListSubscribers(listId, cursor2)
// 	// 	val subscribers = pageOfSubscribers.toArray.map(_.getScreenName)
// 	// 	subscribers.foreach { name => 
// 	// 		println(name)
// 	// 		listOfTuples = name :: listOfSubscribers
// 	// 	}
// 	// 	cursor2 = pageOfSubscribers.getNextCursor
// 	// 	if (pageOfSubscribers.getRateLimitStatus.getRemaining() == 0)
// 	// 		Thread.sleep(pageOfSubscribers.getRateLimitStatus.getSecondsUntilReset() * 1000 + 5000)
// 	// }

// // get statuses


// 	val goalNumber = 1000
// 	var page = new Paging(1,200)
// 	var tweetsResponseList = twitter.getUserListStatuses(listId, page)
// 	var tweetsList = tweetsResponseList.iterator.toList
// 	var fullTweetsList = tweetsList
// 	var lengthOfCurrentTweetsList = tweetsList.length
// 	var numOfTweetsSoFar = lengthOfCurrentTweetsList
// 	var maxIdForNewPaging:Long = 1000000000000000L

// 	var RLstatus_RL = tweetsResponseList.getRateLimitStatus
// 	var rem_RL = RLstatus_RL.getRemaining()
// 	if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}

	
// 	while (lengthOfCurrentTweetsList == 200 && numOfTweetsSoFar < goalNumber ) {
// 		maxIdForNewPaging = tweetsList.map(status => status.getId).sorted.head
// 		page = (new Paging(1,200)).maxId(maxIdForNewPaging)
// 		tweetsResponseList = twitter.getUserTimeline(listId, page)
// 		tweetsList = tweetsResponseList.iterator.toList
// 		lengthOfCurrentTweetsList = tweetsList.length

// 		tweetsList.foreach{ tweet =>
// 			fullTweetsList = tweet :: fullTweetsList
// 		}

// 		numOfTweetsSoFar = fullTweetsList.length

// 		RLstatus_RL = tweetsResponseList.getRateLimitStatus
// 		rem_RL = RLstatus_RL.getRemaining()
// 		if (rem_RL == 0) {Thread.sleep(RLstatus_RL.getSecondsUntilReset() * 1000 + 1000)}

// 	}

// // prints all tweets to terminal

// 	println("\n------- the tweets of " + slug + " ---------")
// 	fullTweetsList.foreach(x => println(createLine(x, name, userLabel.fullString)))

// // prints all tweets to files by List
// 	printToFile(new File("data/corpus/byList/" + slug + ".txt"))(p => {
//  			fullTweetsList.foreach(x=> p.println(createLine(x, name, userLabel.fullString)))
//  		})




// // getUserListMembers(listid, cursor)
// // getUserListSubscribers(listid, cursor)

	

// 	def main(args: Array[String]):Unit = {
// 		//println("works")
// 		//getTweets("evans_anlp ps.hi.hi-health.mid:herb.mid-relig.mid:sup.mid")
// 		listOfMembers.foreach(println)
// 	}
// }

