package thesiscode.util

import nak.data.FeatureObservation
import java.io.File
import scala.io.Source
import play.api.libs.json._
import chalk.lang.eng.PorterStemmer




object Util {

	val stemmer = new PorterStemmer

/** 
used like this:

import java.io._
val data = Array("Five","strings","in","a","file!")

printToFile(new File("example.txt"))(p => {
  data.foreach(p.println)
})
**/
	
	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
		  val p = new java.io.PrintWriter(f)
		  try { op(p) } finally { p.close() }
	}

	def getFirstLine(f: File): String = {
	  Source.fromFile(f).getLines.next
	 }


////////////////////// text-cleaning functions ////////////

	def getText (
		jsonString: String, 
		keepHash: Boolean,
		keepPunct: Boolean,
		keepEmoji: Boolean,
		keepMentions: Boolean = false
		//keepContractions: Boolean = false
		) = {

			val tweetJson = Json.parse(jsonString)
			val retweetedStatusText = (tweetJson \ "retweeted_status" \ "text").asOpt[String]
			val json = if (retweetedStatusText == None) {
				tweetJson
				} else {
				(tweetJson \ "retweeted_status")
				}
			var text = (json \ "text").as[String]


			//find out what hyperlink urls were in the tweet using the json info
			val urls: Seq[String] = (json \\ "url").map(_.asOpt[String]).flatten

			// remove hyperlink URLs from tweet text
			urls.foreach{url => 
				text = text.replaceAllLiterally(url, " ")
			}

			// rid emoji
			if (!keepEmoji) {
				text = ridEmoji(text)
			}

			//toLowercase
			text = text.toLowerCase

			// AMP

			text = text.replaceAll("&amp;", " and ")

			//rid"RT"

			text = ridRT(text)

			text = ridMT(text)


			//find inactive URLs
			val inactiveUrls = text.split("\\s+").filter(x => x.contains("bit.ly")||x.contains("t.co")||x.contains("http:")||x.contains("https:")||x.contains(".com")||x.contains("ow.ly")||x.contains(".net")||x.contains("www.")||x.contains("tinyurl")||x.contains(".org"))
			inactiveUrls.toList.sortBy(x=>x.length).reverse.foreach{url => 
				text = text.replaceAllLiterally(url, " ")
			}

			//rid mentions
			if (!keepMentions) {
				text = ridMentions(text)
			}

			// rid smileys
			text = ridSmileys(text)

			//rid hashtags
			if (!keepHash) {
				text = ridHashtags(text)
			}

			//rid or separate punctuation
			if (!keepPunct) {
				text = ridPunct(text)
			} else {
				text = spacePunct(text)
			}


			//turn all whitespace into a single space
			text = text.replaceAll("\\s+", " ")

			//return
			text.trim
	}

















	def getTextKeepAll (
		jsonString: String
		) = {

			val tweetJson = Json.parse(jsonString)
			val retweetedStatusText = (tweetJson \ "retweeted_status" \ "text").asOpt[String]
			val json = if (retweetedStatusText == None) {
				tweetJson
				} else {
				(tweetJson \ "retweeted_status")
				}
			var text = (json \ "text").as[String]


			//turn all whitespace into a single space
			text = text.replaceAll("\\s+", " ")

			//return
			text.trim
	}
























def getTextNoStopwords (
		jsonString: String, 
		keepHash: Boolean,
		keepPunct: Boolean,
		keepEmoji: Boolean = false,
		keepMentions: Boolean = false
		//keepContractions: Boolean = false
		) = {

			val tweetJson = Json.parse(jsonString)
			val retweetedStatusText = (tweetJson \ "retweeted_status" \ "text").asOpt[String]
			val json = if (retweetedStatusText == None) {
				tweetJson
				} else {
				(tweetJson \ "retweeted_status")
				}
			var text = (json \ "text").as[String]


			//find out what hyperlink urls were in the tweet using the json info
			val urls: Seq[String] = (json \\ "url").map(_.asOpt[String]).flatten

			// remove hyperlink URLs from tweet text
			urls.foreach{url => 
				text = text.replaceAllLiterally(url, " ")
			}

			// rid emoji
			if (!keepEmoji) {
				text = ridEmoji(text)
			}


			//toLowercase
			text = text.toLowerCase


			// AMP

			text = text.replaceAll("&amp;", " and ")

			//rid "RT"

			text = ridRT(text)

			//rid "MT"
			text = ridMT(text)


			//find inactive URLs
			val inactiveUrls = text.split("\\s+").filter(x => x.contains("bit.ly")||x.contains("t.co")||x.contains("http:")||x.contains("https:")||x.contains(".com")||x.contains("ow.ly")||x.contains(".net")||x.contains("www.")||x.contains("tinyurl")||x.contains(".org"))
			inactiveUrls.toList.sortBy(x=>x.length).reverse.foreach{url => 
				text = text.replaceAllLiterally(url, " ")
			}

			//rid mentions
			if (!keepMentions) {
				text = ridMentions(text)
			}

			// rid smileys
			text = ridSmileys(text)

			//rid hashtags
			if (!keepHash) {
				text = ridHashtags(text)
			}

			//rid or separate punctuation
			if (!keepPunct) {
				text = ridPunct(text)
			} else {
				text = spacePunct(text)
			}

			//println(text)

			//rid stopwords
			text = text.split("\\s+").filterNot(thesiscode.classify.ClassifyUtil.stopwords).map(_.trim).mkString(" ")



			//return
			text
	}





















def cleanText (
		basicText: String, 
		keepHash: Boolean,
		keepPunct: Boolean,
		keepEmoji: Boolean,
		keepMentions: Boolean
		) = {

			var text = basicText

			//text = text.replaceAll("&amp", " and ")

			//rid mentions
			if (!keepMentions) {
				text = ridMentions(text)
			}


			// rid emoji
			if (!keepEmoji) {
				text = ridEmoji(text)
			}

			// rid smileys
			text = ridSmileys(text)

			//rid hashtags
			if (!keepHash) {
				text = ridHashtags(text)
			}

			//toLowercase
			text = text.toLowerCase

			//rid or separate punctuation
			if (!keepPunct) {
				if (!keepHash) {text = ridPunct(text)} else {text = ridPunctKeepHash(text)}
			} else {
				if (!keepHash) {text = spacePunct(text)} else {text = spacePunctKeepHash(text)}
			}

			//rid minus signs
			text = ridMinusSigns(text)


			//turn all space/tab sequences into a single space
			text = text.replaceAll("""[ \t]+""", " ")

			//return
			text.trim

	}




	def ridMinusSigns (txt: String) = {
		val lines = txt.split("\n")
		val cleanedLines = lines.map{ line =>
			val split = line.split("\\s+")
			val filtered = split.filterNot(x => (x.startsWith("-")||x.endsWith("-")))
			filtered.mkString(" ")
		}
		cleanedLines.mkString("\n")
	}



	def ridRT(txt: String) =
			txt.split("\n").map{ line =>
				line.split("\\s+").filterNot(x=>x=="rt").mkString(" ")
				}.mkString("\n")

	def ridMT(txt: String) =
			txt.split("\n").map{ line =>
				line.split("\\s+").filterNot(x=>x=="mt").mkString(" ")
				}.mkString("\n")



	val emoji = Source.fromFile("src/main/resources/emojiFile.txt").mkString.split("\\s+")

	def ridEmoji (txt: String) = {
		var t = txt
		t = t.replaceAll("©", "")
		emoji.foreach{ emoji =>
			t = t.replaceAll(emoji, " ")
		}
		t.trim
	}

	def ridMentions (txt: String) = {
		val MentionRE = """(?:\W|^)@\w+""".r
		MentionRE.replaceAllIn(txt, " ").trim
	}

	def ridHashtags (txt: String) = {
		val HashRE = """(?:\W|^)#\w+""".r
		HashRE.replaceAllIn(txt, " ").trim
	}


	def ridSmileys (txt: String) = {
		val SmileyRE = """(O_o|O\.o|:-?[\)\(]|:-?D|;-?[\)\*]|B-?\)|[:;]-?\*|>:-?[\(\)]|:-?[\\/])""".r
		SmileyRE.replaceAllIn(txt, " ").trim
	}


	def ridNumberLetterMixes (txt: String) = {
		val HashRE = """\W\w*(?:[A-Za-z]\d|\d[A-Za-z])\w*\W""".r
		HashRE.replaceAllIn(txt, " ").trim
	}

	def spacePunctRC(raw: String): String = {
		raw.replaceAll("""([#&\*\"\|\[\]\(\)\{\},;:'!\?;:\.“”/\|\\]|--+|<|>)""", " $1 ")
			.trim
	}

	def spacePunct(raw: String): String = {
		raw.replaceAll("""([#&\*\"\|\[\]\(\)\{\},;:!\?;:\.“”/\|\\]|--+|<|>)""", " $1 ")
			.replaceAll("""(?:\W|^)'(\w+)'\W?""", """ ' $1 ' """)
			.trim
	}

	def spacePunctKeepHash(raw: String): String = {
		raw.replaceAll("""([&\*\"\|\[\]\(\)\{\},;:!\?;:\.“”/\|\\]|--+|<|>)""", " $1 ")
			.replaceAll("""(?:\W|^)'(\w+)'\W?""", """ ' $1 ' """)
			.trim
	}

	def ridPunctRC(raw: String): String = {
		raw.replaceAll("""([#&\*\"\|\[\]\(\)\{\},;:'!\?;:\.“”/\|\\]|--+|<|>)""", " ")
			.replaceAll("""([#&\*\"\|\[\]\(\)\{\},;:'!\?;:\.“”/\|\\]|--+|<|>)""", "")
			.trim
	}

	def ridPunct(raw: String): String = {
		raw.replaceAll("""([#&\*\"\|\[\]\(\)\{\},;:!\?;:\.“”/\|\\]|--+|<|>)""", " ")
			.replaceAll("""([#&\*\"\|\[\]\(\)\{\},;:!\?;:\.“”/\|\\]|--+|<|>)""", "")
			.replaceAll("""(?:\W|^)'(\w+)'\W?""", " $1 ").trim
	}

	def ridPunctKeepHash(raw: String): String = {
		raw.replaceAll("""([&\*\"\|\[\]\(\)\{\},;:!\?;:\.“”/\|\\]|--+|<|>)""", " ")
			.replaceAll("""([&\*\"\|\[\]\(\)\{\},;:!\?;:\.“”/\|\\]|--+|<|>)""", "")
			.replaceAll("""(?:\W|^)'(\w+)'\W?""", " $1 ").trim
	}


	// def ridPuncLow(raw: String): String = { //doesn't get rid of sentence-dividing punctuation
	// 	//raw.replaceAll("""([\?!\";\|\[\].,'])""", " $1 ").trim
	// 	raw.replaceAll("""([\"\|\[\]\(\),!\?;\.“”]|--+)""", " $1 ").trim
	// 		.replaceAll("""([\"\|\[\]\(\),]|--+)""", "")
	// }





	def main (args: Array[String]) {


	}




}















	// def ridEmoji (text: String) = {
	// 	//val Emoji = "[^\\x00-\\x7f-\\x80-\\xad]".r
	// 	//val Emoji = """[\u0001F300-\u0001F5FF]""".r
	// 	var t = text
	// 	//U+1F300–U+1F5FF
	// 	//val noEmo = Emoji.replaceAllIn(text, " ")
	// 	// val emoji = Source.fromFile("emojiFile.txt").mkString.split("\\s+")
	// 	t = t.replaceAll("©", "")
	// 	emoji.foreach{ emoji =>
	// 		t = t.replaceAll(emoji, " ")
	// 	}
	// 	//t = Emoji.replaceAllIn(t, "")
	// 	t.trim
	// }


	// def ridSmileys (text: String) = {
	// 	val SmileyRE = """(O_o|O\.o|:-?[\)\(]|:-?D|;-?[\)\*]|B-?\)|[:;]-?\*|>:-?[\(\)]|:-?[\\/])""".r
	// 	SmileyRE.replaceAllIn(text, " ").trim
	// 	//"""(O_o|O\.o|:-?[\)\(]|:-?D|;-?[\)\*]|B-?\)|[:;]-?\*|>:-?[\(\)])"""
	// 	//""">?[=:;B]-[\)\(D0oO\\/\*]|O_o|o_O|O\.o|>?[=:;B]-[\)\(DP0oO\\/\*]"""
	// }
	




	/////////////////////////////// non-JSON corpus stuff /////////////////////////

	// def getTweetLineText(tweetLine: String): String = {
	// 	tweetLine.split(",").drop(9).mkString(",")
	// }

	// def getTweetFileText(tweetFile: File): String = {
	// 	val lines = Source.fromFile(tweetFile).getLines
	// 	lines.next
	// 	val textLines = lines.map(getTweetLineText)
	// 	textLines.mkString("\n")
	// }

	// def getBasicText(rawText: String): String = {
	// 	val rawLines = rawText.split("\n")
	// 	rawLines.map{line =>
	// 		line.split(",").drop(9).mkString(",")
	// 	}.mkString("\n")
	// }

	// def ridCutoffs (text: String) = {
	// 	val lineArray = text.split("\n")
	// 	val arrayArray = lineArray.map(_.split("\\s+"))
	// 	val rid = arrayArray.map{arrayLine =>
	// 		val opt = arrayLine.lastOption
	// 		if (opt==None) {
	// 				arrayLine
	// 			} else if (opt.get.endsWith("…") && opt.get != "…") {
	// 				arrayLine.dropRight(1)
	// 			} else {
	// 				arrayLine
	// 			}
	// 		}
	// 	rid.map(array => array.mkString(" ")).mkString("\n")

	// }

	// def getUnigrams (tokens: Array[String]): Seq[nak.data.FeatureObservation[String]] = {
	// 	val filtered = tokens.filterNot(_=="#").toSet.toList
	// 	    //.filterNot(x => x.contains(""))
	// 	filtered.map(tok => FeatureObservation("word="+tok))
	// }

	// def getBigrams (tokens: Array[String]): Seq[nak.data.FeatureObservation[String]] = {
	// 	val arrayArray = tokens.sliding(2).toArray
	// 	val stringArray = arrayArray.map(arr => "bi=" + arr.mkString("_"))
	// 	stringArray.map(bigram => FeatureObservation(bigram))
	// }

	// def getHashtagFeatures (tokens: Array[String]) = {
	// 	val hashTags = tokens.filter(_.startsWith("#"))
	// 	val bares = hashTags.map(tag => tag.drop(1))
	// 	val longEnoughs = bares.filter(_.length >= 4)
	// 	val threes = bares.sliding(3).toList
	// 	val fours = bares.sliding(4).toList
	// }
////////////////////////








		// val json1 = """{"retweeted_status":{"contributors":null,"text":"The Best Caption Ever!......#Catholic\nhttp://t.co/D4Fsv6tS8D","geo":null,"retweeted":true,"in_reply_to_screen_name":null,"possibly_sensitive":false,"truncated":false,"lang":"en","entities":{"symbols":[],"urls":[],"hashtags":[{"text":"Catholic","indices":[28,37]}],"media":[{"sizes":{"small":{"w":340,"resize":"fit","h":261},"thumb":{"w":150,"resize":"crop","h":150},"medium":{"w":600,"resize":"fit","h":461},"large":{"w":650,"resize":"fit","h":499}},"id":360797385317834752,"media_url_https":"https://pbs.twimg.com/media/BQHPT3PCYAAZ0ma.jpg","media_url":"http://pbs.twimg.com/media/BQHPT3PCYAAZ0ma.jpg","expanded_url":"http://twitter.com/GoodWithoutGods/status/360797385313640448/photo/1","source_status_id_str":"360797385313640448","indices":[38,60],"source_status_id":360797385313640448,"id_str":"360797385317834752","type":"photo","display_url":"pic.twitter.com/D4Fsv6tS8D","url":"http://t.co/D4Fsv6tS8D"}],"user_mentions":[]},"in_reply_to_status_id_str":null,"id":444912315872583680,"source":"web","in_reply_to_user_id_str":null,"favorited":false,"in_reply_to_status_id":null,"retweet_count":23,"created_at":"Sat Mar 15 19:05:36 +0000 2014","in_reply_to_user_id":null,"favorite_count":19,"id_str":"444912315872583680","place":null,"user":{"location":"Snowdonia, Cymru, UK.","default_profile":false,"profile_background_tile":true,"statuses_count":11341,"lang":"en","profile_link_color":"0099B9","profile_banner_url":"https://pbs.twimg.com/profile_banners/117763834/1390500914","id":117763834,"following":false,"protected":false,"favourites_count":11930,"profile_text_color":"3C3940","description":"Woods and Books - things i like. PhD. Freethinker.  History, science, peace, nature and beauty. Charles Darwin is my slingshot.","verified":false,"contributors_enabled":false,"profile_sidebar_border_color":"5ED4DC","name":"Woods and Books","profile_background_color":"0099B9","created_at":"Fri Feb 26 14:50:13 +0000 2010","is_translation_enabled":false,"default_profile_image":false,"followers_count":8063,"profile_image_url_https":"https://pbs.twimg.com/profile_images/2445530919/wx6o78d9ye68c662gdaw_normal.jpeg","geo_enabled":false,"profile_background_image_url":"http://pbs.twimg.com/profile_background_images/604066313/4xf5ai5t6quamyb0298k.jpeg","profile_background_image_url_https":"https://pbs.twimg.com/profile_background_images/604066313/4xf5ai5t6quamyb0298k.jpeg","follow_request_sent":false,"entities":{"description":{"urls":[]},"url":{"urls":[{"expanded_url":"http://www.woodlandbookshop.blogspot.co.uk/","indices":[0,22],"display_url":"woodlandbookshop.blogspot.co.uk","url":"http://t.co/as7cHUcdbE"}]}},"url":"http://t.co/as7cHUcdbE","utc_offset":0,"time_zone":"London","notifications":false,"profile_use_background_image":true,"friends_count":6719,"profile_sidebar_fill_color":"95E8EC","screen_name":"Woodlandbookshp","id_str":"117763834","profile_image_url":"http://pbs.twimg.com/profile_images/2445530919/wx6o78d9ye68c662gdaw_normal.jpeg","listed_count":246,"is_translator":false},"coordinates":null},"contributors":null,"text":"RT @Woodlandbookshp: The Best Caption Ever!......#Catholic\nhttp://t.co/D4Fsv6tS8D","geo":null,"retweeted":true,"in_reply_to_screen_name":null,"possibly_sensitive":false,"truncated":false,"lang":"en","entities":{"symbols":[],"urls":[],"hashtags":[{"text":"Catholic","indices":[49,58]}],"media":[{"sizes":{"small":{"w":340,"resize":"fit","h":261},"thumb":{"w":150,"resize":"crop","h":150},"medium":{"w":600,"resize":"fit","h":461},"large":{"w":650,"resize":"fit","h":499}},"id":360797385317834752,"media_url_https":"https://pbs.twimg.com/media/BQHPT3PCYAAZ0ma.jpg","media_url":"http://pbs.twimg.com/media/BQHPT3PCYAAZ0ma.jpg","expanded_url":"http://twitter.com/GoodWithoutGods/status/360797385313640448/photo/1","source_status_id_str":"360797385313640448","indices":[59,81],"source_status_id":360797385313640448,"id_str":"360797385317834752","type":"photo","display_url":"pic.twitter.com/D4Fsv6tS8D","url":"http://t.co/D4Fsv6tS8D"}],"user_mentions":[{"id":117763834,"name":"Woods and Books","indices":[3,19],"screen_name":"Woodlandbookshp","id_str":"117763834"}]},"in_reply_to_status_id_str":null,"id":444941244448272384,"source":"web","in_reply_to_user_id_str":null,"favorited":false,"in_reply_to_status_id":null,"retweet_count":23,"created_at":"Sat Mar 15 21:00:33 +0000 2014","in_reply_to_user_id":null,"favorite_count":0,"id_str":"444941244448272384","place":null,"user":{"location":"","default_profile":true,"profile_background_tile":false,"statuses_count":298,"lang":"en","profile_link_color":"0084B4","id":1123605330,"following":false,"protected":false,"favourites_count":0,"profile_text_color":"333333","description":"This was used for a project. So far I haven't started using it like a normal account.","verified":false,"contributors_enabled":false,"profile_sidebar_border_color":"C0DEED","name":"James Evans","profile_background_color":"C0DEED","created_at":"Sun Jan 27 01:07:37 +0000 2013","is_translation_enabled":false,"default_profile_image":true,"followers_count":13,"profile_image_url_https":"https://abs.twimg.com/sticky/default_profile_images/default_profile_1_normal.png","geo_enabled":false,"profile_background_image_url":"http://abs.twimg.com/images/themes/theme1/bg.png","profile_background_image_url_https":"https://abs.twimg.com/images/themes/theme1/bg.png","follow_request_sent":false,"entities":{"description":{"urls":[]}},"url":null,"utc_offset":-14400,"time_zone":"Eastern Time (US & Canada)","notifications":false,"profile_use_background_image":true,"friends_count":23,"profile_sidebar_fill_color":"DDEEF6","screen_name":"evans_anlp","id_str":"1123605330","profile_image_url":"http://abs.twimg.com/sticky/default_profile_images/default_profile_1_normal.png","listed_count":0,"is_translator":false},"coordinates":null}"""

		// val json2 = """{"contributors":null,"text":"test tweet. http://t.co/XORueLN3J7","geo":null,"retweeted":false,"in_reply_to_screen_name":null,"possibly_sensitive":false,"truncated":false,"lang":"fr","entities":{"symbols":[],"urls":[{"expanded_url":"http://whatstheharm.net","indices":[12,34],"display_url":"whatstheharm.net","url":"http://t.co/XORueLN3J7"}],"hashtags":[],"user_mentions":[]},"in_reply_to_status_id_str":null,"id":447050262763405312,"source":"web","in_reply_to_user_id_str":null,"favorited":false,"in_reply_to_status_id":null,"retweet_count":0,"created_at":"Fri Mar 21 16:41:03 +0000 2014","in_reply_to_user_id":null,"favorite_count":0,"id_str":"447050262763405312","place":null,"user":{"location":"","default_profile":true,"profile_background_tile":false,"statuses_count":298,"lang":"en","profile_link_color":"0084B4","id":1123605330,"following":false,"protected":false,"favourites_count":0,"profile_text_color":"333333","description":"This was used for a project. So far I haven't started using it like a normal account.","verified":false,"contributors_enabled":false,"profile_sidebar_border_color":"C0DEED","name":"James Evans","profile_background_color":"C0DEED","created_at":"Sun Jan 27 01:07:37 +0000 2013","is_translation_enabled":false,"default_profile_image":true,"followers_count":13,"profile_image_url_https":"https://abs.twimg.com/sticky/default_profile_images/default_profile_1_normal.png","geo_enabled":false,"profile_background_image_url":"http://abs.twimg.com/images/themes/theme1/bg.png","profile_background_image_url_https":"https://abs.twimg.com/images/themes/theme1/bg.png","follow_request_sent":false,"entities":{"description":{"urls":[]}},"url":null,"utc_offset":-14400,"time_zone":"Eastern Time (US & Canada)","notifications":false,"profile_use_background_image":true,"friends_count":23,"profile_sidebar_fill_color":"DDEEF6","screen_name":"evans_anlp","id_str":"1123605330","profile_image_url":"http://abs.twimg.com/sticky/default_profile_images/default_profile_1_normal.png","listed_count":0,"is_translator":false},"coordinates":null}"""

		// val json3 = """{"created_at":"Mon Jun 10 21:36:31 +0000 2013","id":344206467924127745,"id_str":"344206467924127745","text":"Luka Modric, \"Todos los ni\u00f1os sue\u00f1an con jugar en el Real Madrid y eso me pasaba a m\u00ed\"","source":"web","truncated":false,"in_reply_to_status_id":null,"in_reply_to_status_id_str":null,"in_reply_to_user_id":null,"in_reply_to_user_id_str":null,"in_reply_to_screen_name":null,"user":{"id":196684948,"id_str":"196684948","name":"Mar0","screen_name":"maro_madrid","location":"","url":null,"description":"MADRIDISTA las 24 horas, los 365 dias del a\u00f1o, un sentimiento hacia un escudo, hacia un club: REAL MADRID! .ANTICULE ETERNO!!... SIEMPRE, HALA MADRID!!","protected":false,"followers_count":2161,"friends_count":848,"listed_count":48,"created_at":"Wed Sep 29 17:38:42 +0000 2010","favourites_count":24,"utc_offset":-16200,"time_zone":"Caracas","geo_enabled":false,"verified":false,"statuses_count":45715,"lang":"es","contributors_enabled":false,"is_translator":false,"profile_background_color":"C0DEED","profile_background_image_url":"http:\/\/a0.twimg.com\/profile_background_images\/334877361\/pasiloooo.jpeg","profile_background_image_url_https":"https:\/\/si0.twimg.com\/profile_background_images\/334877361\/pasiloooo.jpeg","profile_background_tile":true,"profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/1134250694\/madrid_cor_normal.jpg","profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/1134250694\/madrid_cor_normal.jpg","profile_banner_url":"https:\/\/pbs.twimg.com\/profile_banners\/196684948\/1357962880","profile_link_color":"0084B4","profile_sidebar_border_color":"C0DEED","profile_sidebar_fill_color":"DDEEF6","profile_text_color":"333333","profile_use_background_image":true,"default_profile":false,"default_profile_image":false,"following":null,"follow_request_sent":null,"notifications":null},"geo":null,"coordinates":null,"place":null,"contributors":null,"retweet_count":0,"favorite_count":0,"entities":{"hashtags":[],"symbols":[],"urls":[],"user_mentions":[]},"favorited":false,"retweeted":false,"filter_level":"medium","lang":"es"}"""

		// val json4 = """{"created_at":"Mon Jun 10 21:36:31 +0000 2013","id":344206467949268992,"id_str":"344206467949268992","text":"RT @Goge22Rodrigo: XXXXX All niggas thirsty","source":"\u003ca href=\"http:\/\/twitter.com\/download\/iphone\" rel=\"nofollow\"\u003eTwitter for iPhone\u003c\/a\u003e","truncated":false,"in_reply_to_status_id":null,"in_reply_to_status_id_str":null,"in_reply_to_user_id":null,"in_reply_to_user_id_str":null,"in_reply_to_screen_name":null,"user":{"id":634235820,"id_str":"634235820","name":"Karen Pi\u00f1on","screen_name":"Nasty_Karen","location":"","url":"http:\/\/www.facebook.com\/karenn.love13","description":"16 years young. Junior at Yucaipa High;p Follow me tho.\u263a","protected":false,"followers_count":185,"friends_count":154,"listed_count":0,"created_at":"Fri Jul 13 00:17:08 +0000 2012","favourites_count":1318,"utc_offset":28800,"time_zone":"Beijing","geo_enabled":true,"verified":false,"statuses_count":7914,"lang":"en","contributors_enabled":false,"is_translator":false,"profile_background_color":"C0DEED","profile_background_image_url":"http:\/\/a0.twimg.com\/profile_background_images\/610611581\/qzyksl141mc8eqnq0blu.jpeg","profile_background_image_url_https":"https:\/\/si0.twimg.com\/profile_background_images\/610611581\/qzyksl141mc8eqnq0blu.jpeg","profile_background_tile":false,"profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/3716091390\/4fed2cf04785f66bc33f755023284a7e_normal.jpeg","profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/3716091390\/4fed2cf04785f66bc33f755023284a7e_normal.jpeg","profile_banner_url":"https:\/\/pbs.twimg.com\/profile_banners\/634235820\/1364529722","profile_link_color":"0084B4","profile_sidebar_border_color":"C0DEED","profile_sidebar_fill_color":"DDEEF6","profile_text_color":"333333","profile_use_background_image":true,"default_profile":false,"default_profile_image":false,"following":null,"follow_request_sent":null,"notifications":null},"geo":null,"coordinates":null,"place":null,"contributors":null,"retweeted_status":{"created_at":"Mon Jun 10 21:34:59 +0000 2013","id":344206080789868545,"id_str":"344206080789868545","text":"@JamesBond @Jesus XXXX niggas thirsty","source":"\u003ca href=\"http:\/\/twitter.com\/download\/iphone\" rel=\"nofollow\"\u003eTwitter for iPhone\u003c\/a\u003e","truncated":false,"in_reply_to_status_id":null,"in_reply_to_status_id_str":null,"in_reply_to_user_id":null,"in_reply_to_user_id_str":null,"in_reply_to_screen_name":null,"user":{"id":493836969,"id_str":"493836969","name":"Goge Salazar","screen_name":"Goge22Rodrigo","location":"california","url":"http:\/\/brc9hoe9.tumblr.com","description":"instagram||gogert\nSubscribe to my pheed|| goge salazar","protected":false,"followers_count":306,"friends_count":356,"listed_count":0,"created_at":"Thu Feb 16 07:38:04 +0000 2012","favourites_count":1832,"utc_offset":null,"time_zone":null,"geo_enabled":true,"verified":false,"statuses_count":6544,"lang":"en","contributors_enabled":false,"is_translator":false,"profile_background_color":"C0DEED","profile_background_image_url":"http:\/\/a0.twimg.com\/images\/themes\/theme1\/bg.png","profile_background_image_url_https":"https:\/\/si0.twimg.com\/images\/themes\/theme1\/bg.png","profile_background_tile":false,"profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/3686288340\/adc98ff45514f62fecb1a1bc119912a4_normal.jpeg","profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/3686288340\/adc98ff45514f62fecb1a1bc119912a4_normal.jpeg","profile_banner_url":"https:\/\/pbs.twimg.com\/profile_banners\/493836969\/1365579191","profile_link_color":"0084B4","profile_sidebar_border_color":"C0DEED","profile_sidebar_fill_color":"DDEEF6","profile_text_color":"333333","profile_use_background_image":true,"default_profile":true,"default_profile_image":false,"following":null,"follow_request_sent":null,"notifications":null},"geo":null,"coordinates":null,"place":null,"contributors":null,"retweet_count":1,"favorite_count":0,"entities":{"hashtags":[],"symbols":[],"urls":[],"user_mentions":[]},"favorited":false,"retweeted":false,"lang":"en"},"retweet_count":0,"favorite_count":0,"entities":{"hashtags":[],"symbols":[],"urls":[],"user_mentions":[{"screen_name":"Goge22Rodrigo","name":"Goge Salazar","id":493836969,"id_str":"493836969","indices":[3,17]}]},"favorited":false,"retweeted":false,"filter_level":"medium","lang":"en"}"""

		// val json5 = """{"created_at":"Mon Jun 10 21:36:31 +0000 2013","id":344206467924099072,"id_str":"344206467924099072","text":"@realmadrid","source":"web","truncated":false,"in_reply_to_status_id":null,"in_reply_to_status_id_str":null,"in_reply_to_user_id":14872237,"in_reply_to_user_id_str":"14872237","in_reply_to_screen_name":"realmadrid","user":{"id":1499337536,"id_str":"1499337536","name":"Altay KO\u00c7","screen_name":"6_altay","location":"","url":null,"description":null,"protected":false,"followers_count":1,"friends_count":24,"listed_count":0,"created_at":"Mon Jun 10 20:43:33 +0000 2013","favourites_count":0,"utc_offset":null,"time_zone":null,"geo_enabled":false,"verified":false,"statuses_count":5,"lang":"tr","contributors_enabled":false,"is_translator":false,"profile_background_color":"C0DEED","profile_background_image_url":"http:\/\/a0.twimg.com\/images\/themes\/theme1\/bg.png","profile_background_image_url_https":"https:\/\/si0.twimg.com\/images\/themes\/theme1\/bg.png","profile_background_tile":false,"profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/3782870966\/3e6070ff2908c17feccfa5c61ca22110_normal.jpeg","profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/3782870966\/3e6070ff2908c17feccfa5c61ca22110_normal.jpeg","profile_link_color":"0084B4","profile_sidebar_border_color":"C0DEED","profile_sidebar_fill_color":"DDEEF6","profile_text_color":"333333","profile_use_background_image":true,"default_profile":true,"default_profile_image":false,"following":null,"follow_request_sent":null,"notifications":null},"geo":null,"coordinates":null,"place":null,"contributors":null,"retweet_count":0,"favorite_count":0,"entities":{"hashtags":[],"symbols":[],"urls":[],"user_mentions":[{"screen_name":"realmadrid","name":"Real Madrid C. F.","id":14872237,"id_str":"14872237","indices":[0,11]}]},"favorited":false,"retweeted":false,"filter_level":"medium","lang":"und"}"""
		
		// val json6 = """{"created_at":"Mon Jun 10 21:36:31 +0000 2013","id":344206467940880385,"id_str":"344206467940880385","text":"RT @_3z_: \u0627\u0648\u0644 \u0645\u0627\u064a\u0646\u062a\u0634\u0631 iOS 7\n\n- \u0645\u0631\u0643\u0628 \u062c\u0644\u0628\u0631\u064a\u0643 \u061f\n\u2013 \u0644\u0627 \u0648\u0627\u0644\u0644\u0647\n- \u0627\u062c\u0644 \u0648\u0634 \u0647\u0627\u0644\u062b\u064a\u0645 \u0627\u0644\u064a \u062d\u0627\u0637\u0647 \u061f\n\u2013 \ud83d\ude11","source":"\u003ca href=\"http:\/\/twitter.com\/download\/iphone\" rel=\"nofollow\"\u003eTwitter for iPhone\u003c\/a\u003e","truncated":false,"in_reply_to_status_id":null,"in_reply_to_status_id_str":null,"in_reply_to_user_id":null,"in_reply_to_user_id_str":null,"in_reply_to_screen_name":null,"user":{"id":291806984,"id_str":"291806984","name":"IBRAHIM - S","screen_name":"Ibra_s_s","location":"U-S-I","url":null,"description":"\u0623\u0639\u062a\u0628\u0631\u0635\u0645\u062a\u064a\u060c\u062c\u0632\u0621 \u0645\u0646 \u0643\u0644\u0627\u0645\u064a.. #\u062d\u0633\u0646_\u0627\u0644\u062e\u0644\u0642 \u0634\u0639\u0627\u0631\u064a:\u0627\u0644\u0627\u062d\u062a\u0631\u0627\u0645 .. \u0623\u0645\u0646\u064a\u062a\u064a:..\u062c\u0646\u0627\u0646 \u0627\u0644\u0631\u062d\u0645\u0646.. \u0648\u0627\u0644\u0627\u0631\u062a\u0642\u0627\u0621 \u0628\u0627\u0644\u0645\u062c\u062a\u0645\u0639 #\u0627\u0631\u062a\u0642\u0648\u0627 \u0645\u0624\u0645\u0646 \u062a\u0645\u0627\u0645 \u0627\u0644\u0627\u064a\u0645\u0627\u0646 \u0628\u0642\u0635\u0629 \u0627\u0644\u0636\u0641\u062f\u0639!","protected":false,"followers_count":1122,"friends_count":374,"listed_count":0,"created_at":"Mon May 02 17:01:14 +0000 2011","favourites_count":1316,"utc_offset":10800,"time_zone":"Riyadh","geo_enabled":true,"verified":false,"statuses_count":13969,"lang":"en","contributors_enabled":false,"is_translator":false,"profile_background_color":"C0DEED","profile_background_image_url":"http:\/\/a0.twimg.com\/profile_background_images\/682316718\/062866e05b7a9e02676db6b310e122f6.jpeg","profile_background_image_url_https":"https:\/\/si0.twimg.com\/profile_background_images\/682316718\/062866e05b7a9e02676db6b310e122f6.jpeg","profile_background_tile":true,"profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/3780155038\/e8d911580a0b50c4a91e980095bb6b2a_normal.jpeg","profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/3780155038\/e8d911580a0b50c4a91e980095bb6b2a_normal.jpeg","profile_banner_url":"https:\/\/pbs.twimg.com\/profile_banners\/291806984\/1370138023","profile_link_color":"0084B4","profile_sidebar_border_color":"FFFFFF","profile_sidebar_fill_color":"DDEEF6","profile_text_color":"333333","profile_use_background_image":true,"default_profile":false,"default_profile_image":false,"following":null,"follow_request_sent":null,"notifications":null},"geo":null,"coordinates":null,"place":null,"contributors":null,"retweeted_status":{"created_at":"Mon Jun 10 20:59:10 +0000 2013","id":344197068111368192,"id_str":"344197068111368192","text":"\u0627\u0648\u0644 \u0645\u0627\u064a\u0646\u062a\u0634\u0631 iOS 7\n\n- \u0645\u0631\u0643\u0628 \u062c\u0644\u0628\u0631\u064a\u0643 \u061f\n\u2013 \u0644\u0627 \u0648\u0627\u0644\u0644\u0647\n- \u0627\u062c\u0644 \u0648\u0634 \u0647\u0627\u0644\u062b\u064a\u0645 \u0627\u0644\u064a \u062d\u0627\u0637\u0647 \u061f\n\u2013 \ud83d\ude11","source":"\u003ca href=\"http:\/\/tapbots.com\/tweetbot\" rel=\"nofollow\"\u003eTweetbot for iOS\u003c\/a\u003e","truncated":false,"in_reply_to_status_id":null,"in_reply_to_status_id_str":null,"in_reply_to_user_id":null,"in_reply_to_user_id_str":null,"in_reply_to_screen_name":null,"user":{"id":155492106,"id_str":"155492106","name":"\u025cz | \u0639\u0640\u0632 \uf8ff ","screen_name":"_3z_","location":"Kingdom Of saudi arabia","url":"http:\/\/Q8app.net","description":"Of amateur Photography,\n I am following all the new Technology,  \nDesigner , Blogger , a user Apple ..\uf8ff  \nI give more than i take.\n\ninstagram : _3z_","protected":false,"followers_count":5647,"friends_count":353,"listed_count":144,"created_at":"Mon Jun 14 08:25:05 +0000 2010","favourites_count":757,"utc_offset":10800,"time_zone":"Riyadh","geo_enabled":true,"verified":false,"statuses_count":51952,"lang":"en","contributors_enabled":false,"is_translator":false,"profile_background_color":"0E0D02","profile_background_image_url":"http:\/\/a0.twimg.com\/profile_background_images\/675905500\/d3337f4a0488aaada2c7e54b6bce1f14.jpeg","profile_background_image_url_https":"https:\/\/si0.twimg.com\/profile_background_images\/675905500\/d3337f4a0488aaada2c7e54b6bce1f14.jpeg","profile_background_tile":false,"profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/2961836605\/1933b5a6bba0037863659292962e0dba_normal.png","profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/2961836605\/1933b5a6bba0037863659292962e0dba_normal.png","profile_banner_url":"https:\/\/pbs.twimg.com\/profile_banners\/155492106\/1361957407","profile_link_color":"895429","profile_sidebar_border_color":"BEDFB6","profile_sidebar_fill_color":"0E0D02","profile_text_color":"39BD91","profile_use_background_image":true,"default_profile":false,"default_profile_image":false,"following":null,"follow_request_sent":null,"notifications":null},"geo":null,"coordinates":null,"place":null,"contributors":null,"retweet_count":6,"favorite_count":0,"entities":{"hashtags":[],"symbols":[],"urls":[],"user_mentions":[]},"favorited":false,"retweeted":false,"lang":"ar"},"retweet_count":0,"favorite_count":0,"entities":{"hashtags":[],"symbols":[],"urls":[],"user_mentions":[{"screen_name":"_3z_","name":"\u025cz | \u0639\u0640\u0632 \uf8ff ","id":155492106,"id_str":"155492106","indices":[3,8]}]},"favorited":false,"retweeted":false,"filter_level":"medium","lang":"ar"}"""
		
		// val jList = List(json1,json2,json3,json4,json5,json6)
		// val texts = jList.map(getText(_,false,false,false))
		// texts.foreach(println)





		// val emoji = Source.fromFile("emojiFile.txt").mkString.split("\\s+")
		// print("[")
		// emoji.dropRigh(1).foreach(e => print(e + "|"))
		// print(emoji.last)
		// println("]")

		//getText(json4, true)

	// println(getText(json1, true))

	// val foo = (Json.parse(json1) \ "text").as[String]
	// println(foo)
	// val bar = (Json.parse(json1) \ "retweeted_status" \ "text").asOpt[String]
	// if (bar == None) println("not a retweet")
	// println(bar)

	// val fullOfEmoji = Source.fromFile("src/main/resources/emojiFile.txt").mkString

	// println(ridEmoji(fullOfEmoji))
	//val test = """{"retweeted_status":{"contributors":null,"text":"Nice! \n#atheist http://t.co/M8dkcUYTV7","geo":null,"retweeted":false,"in_reply_to_screen_name":null,"possibly_sensitive":false,"truncated":false,"lang":"de","entities":{"symbols":[],"urls":[],"hashtags":[{"text":"atheist","indices":[7,15]}],"media":[{"sizes":{"small":{"w":340,"resize":"fit","h":226},"thumb":{"w":150,"resize":"crop","h":150},"large":{"w":936,"resize":"fit","h":623},"medium":{"w":600,"resize":"fit","h":399}},"id":410937946204680192,"media_url_https":"https://pbs.twimg.com/media/BbPx4oRCMAAg4X5.jpg","media_url":"http://pbs.twimg.com/media/BbPx4oRCMAAg4X5.jpg","expanded_url":"http://twitter.com/MamaAtheist/status/410937946313719808/photo/1","indices":[16,38],"id_str":"410937946204680192","type":"photo","display_url":"pic.twitter.com/M8dkcUYTV7","url":"http://t.co/M8dkcUYTV7"}],"user_mentions":[]},"in_reply_to_status_id_str":null,"id":410937946313719808,"source":"<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone<\/a>","in_reply_to_user_id_str":null,"favorited":false,"in_reply_to_status_id":null,"retweet_count":10,"created_at":"Thu Dec 12 01:03:35 +0000 2013","in_reply_to_user_id":null,"favorite_count":14,"id_str":"410937946313719808","place":null,"user":{"location":"","default_profile":true,"profile_background_tile":false,"statuses_count":12976,"lang":"en","profile_link_color":"0084B4","profile_banner_url":"https://pbs.twimg.com/profile_banners/1730962201/1389484209","id":1730962201,"following":false,"protected":false,"favourites_count":19296,"profile_text_color":"333333","description":"~ ~ ~ Only the fool says in his heart: There is no god. The wise says it to the world. ~ ~ ~ Sassy atheist. Lots of pics. Dont take you seriously.","verified":false,"contributors_enabled":false,"profile_sidebar_border_color":"C0DEED","name":"Mama A","profile_background_color":"C0DEED","created_at":"Thu Sep 05 05:39:52 +0000 2013","is_translation_enabled":false,"default_profile_image":false,"followers_count":3853,"profile_image_url_https":"https://pbs.twimg.com/profile_images/443221663505588224/pL2XuDq6_normal.jpeg","geo_enabled":false,"profile_background_image_url":"http://abs.twimg.com/images/themes/theme1/bg.png","profile_background_image_url_https":"https://abs.twimg.com/images/themes/theme1/bg.png","follow_request_sent":false,"entities":{"description":{"urls":[]},"url":{"urls":[{"expanded_url":"http://ask.fm/MamaAtheist","indices":[0,22],"display_url":"ask.fm/MamaAtheist","url":"http://t.co/FJy1d1b6fY"}]}},"url":"http://t.co/FJy1d1b6fY","utc_offset":-25200,"time_zone":"Pacific Time (US & Canada)","notifications":false,"profile_use_background_image":true,"friends_count":785,"profile_sidebar_fill_color":"DDEEF6","screen_name":"MamaAtheist","id_str":"1730962201","profile_image_url":"http://pbs.twimg.com/profile_images/443221663505588224/pL2XuDq6_normal.jpeg","listed_count":93,"is_translator":false},"coordinates":null},"contributors":null,"text":"RT @MamaAtheist: Nice! \n#atheist http://t.co/M8dkcUYTV7","geo":null,"retweeted":false,"in_reply_to_screen_name":null,"possibly_sensitive":false,"truncated":false,"lang":"de","entities":{"symbols":[],"urls":[],"hashtags":[{"text":"atheist","indices":[24,32]}],"media":[{"sizes":{"small":{"w":340,"resize":"fit","h":226},"thumb":{"w":150,"resize":"crop","h":150},"large":{"w":936,"resize":"fit","h":623},"medium":{"w":600,"resize":"fit","h":399}},"id":410937946204680192,"media_url_https":"https://pbs.twimg.com/media/BbPx4oRCMAAg4X5.jpg","media_url":"http://pbs.twimg.com/media/BbPx4oRCMAAg4X5.jpg","expanded_url":"http://twitter.com/MamaAtheist/status/410937946313719808/photo/1","indices":[33,55],"id_str":"410937946204680192","type":"photo","display_url":"pic.twitter.com/M8dkcUYTV7","url":"http://t.co/M8dkcUYTV7"}],"user_mentions":[{"id":1730962201,"name":"Mama A","indices":[3,15],"screen_name":"MamaAtheist","id_str":"1730962201"}]},"in_reply_to_status_id_str":null,"id":410941703797735424,"source":"<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone<\/a>","in_reply_to_user_id_str":null,"favorited":false,"in_reply_to_status_id":null,"retweet_count":10,"created_at":"Thu Dec 12 01:18:31 +0000 2013","in_reply_to_user_id":null,"favorite_count":0,"id_str":"410941703797735424","place":null,"user":{"location":"","default_profile":true,"profile_background_tile":false,"statuses_count":2249,"lang":"en","profile_link_color":"0084B4","id":1869180102,"following":false,"protected":false,"favourites_count":1030,"profile_text_color":"333333","description":"Raised catholic, spent 20s and 30s looking at other religions Most infuential person Christopher Hitchens","verified":false,"contributors_enabled":false,"profile_sidebar_border_color":"C0DEED","name":"Dave","profile_background_color":"C0DEED","created_at":"Sun Sep 15 21:06:16 +0000 2013","is_translation_enabled":false,"default_profile_image":false,"followers_count":529,"profile_image_url_https":"https://pbs.twimg.com/profile_images/378800000597672554/d516ce4bfcf16f50d46eeb343120ffb6_normal.jpeg","geo_enabled":false,"profile_background_image_url":"http://abs.twimg.com/images/themes/theme1/bg.png","profile_background_image_url_https":"https://abs.twimg.com/images/themes/theme1/bg.png","follow_request_sent":false,"entities":{"description":{"urls":[]}},"url":null,"utc_offset":-10800,"time_zone":"Atlantic Time (Canada)","notifications":false,"profile_use_background_image":true,"friends_count":552,"profile_sidebar_fill_color":"DDEEF6","screen_name":"CanuckAtheist","id_str":"1869180102","profile_image_url":"http://pbs.twimg.com/profile_images/378800000597672554/d516ce4bfcf16f50d46eeb343120ffb6_normal.jpeg","listed_count":7,"is_translator":false},"coordinates":null}"""
	
	//val foo = """{"contributors":null,"text":"Love planet Venus opposes Pluto & sextiles Jupiter: More info than I can write here!Go to my site & to Weekly Forecast http://bit.ly/9GFVMU","geo":null,"retweeted":false,"in_reply_to_screen_name":null,"truncated":false,"lang":"en","entities":{"symbols":[],"urls":[],"hashtags":[],"user_mentions":[]},"in_reply_to_status_id_str":null,"id":89371217907302400,"source":"<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck<\/a>","in_reply_to_user_id_str":null,"favorited":false,"in_reply_to_status_id":null,"retweet_count":2,"created_at":"Fri Jul 08 16:32:13 +0000 2011","in_reply_to_user_id":null,"favorite_count":1,"id_str":"89371217907302400","place":null,"user":{"location":"USA","default_profile":false,"profile_background_tile":false,"statuses_count":1748,"lang":"en","profile_link_color":"095B69","id":34817166,"following":false,"protected":false,"favourites_count":0,"profile_text_color":"2D2933","description":"Daily Astrology with Vivian Owen","verified":false,"contributors_enabled":false,"profile_sidebar_border_color":"1A294D","name":"Daily Star Tweets","profile_background_color":"0A224A","created_at":"Fri Apr 24 02:26:54 +0000 2009","is_translation_enabled":false,"default_profile_image":false,"followers_count":2858,"profile_image_url_https":"https://pbs.twimg.com/profile_images/259611639/sun_46_normal.gif","geo_enabled":false,"profile_background_image_url":"http://pbs.twimg.com/profile_background_images/48454578/DSC_0091_edited-1.jpg","profile_background_image_url_https":"https://pbs.twimg.com/profile_background_images/48454578/DSC_0091_edited-1.jpg","follow_request_sent":false,"entities":{"description":{"urls":[]}},"url":null,"utc_offset":-25200,"time_zone":"Arizona","notifications":false,"profile_use_background_image":true,"friends_count":239,"profile_sidebar_fill_color":"FAF5C3","screen_name":"DailyStarTweets","id_str":"34817166","profile_image_url":"http://pbs.twimg.com/profile_images/259611639/sun_46_normal.gif","listed_count":98,"is_translator":false},"coordinates":null}"""

	//val foo = """{"contributors":null,"text":"@JimiJamm RT Sorry--hoot didn't work for some reason. Here's a bit.ly link: http://bit.ly/a1lEJI","geo":null,"retweeted":false,"in_reply_to_screen_name":"jimijamm","truncated":false,"lang":"en","entities":{"symbols":[],"urls":[],"hashtags":[],"user_mentions":[{"id":19617590,"name":"Jimi Jamm","indices":[0,9],"screen_name":"jimijamm","id_str":"19617590"}]},"in_reply_to_status_id_str":"9793659743","id":9823618271,"source":"<a href=\"http://www.hootsuite.com\" rel=\"nofollow\">HootSuite<\/a>","in_reply_to_user_id_str":"19617590","favorited":false,"in_reply_to_status_id":9793659743,"retweet_count":0,"created_at":"Mon Mar 01 13:01:03 +0000 2010","in_reply_to_user_id":19617590,"favorite_count":0,"id_str":"9823618271","place":null,"user":{"location":"Minneapolis, MN","default_profile":false,"profile_background_tile":true,"statuses_count":3083,"lang":"en","profile_link_color":"0048FF","id":14809486,"following":false,"protected":false,"favourites_count":1014,"profile_text_color":"000000","description":"I'm a Voiceover coach and talent, Belly dance teacher and performer. I love my husband @QuantumGood, our cats & spiritual freedom!","verified":false,"contributors_enabled":false,"profile_sidebar_border_color":"87BC44","name":"Sarah Jones-Larson","profile_background_color":"FFFFFF","created_at":"Sat May 17 09:50:28 +0000 2008","is_translation_enabled":false,"default_profile_image":false,"followers_count":4489,"profile_image_url_https":"https://pbs.twimg.com/profile_images/1023303425/150_color_normal.jpg","geo_enabled":false,"profile_background_image_url":"http://pbs.twimg.com/profile_background_images/3183443/montageLessSharp.jpg","profile_background_image_url_https":"https://pbs.twimg.com/profile_background_images/3183443/montageLessSharp.jpg","follow_request_sent":false,"entities":{"description":{"urls":[]},"url":{"urls":[{"expanded_url":"http://sarah-jones-larson.blogspot.com/search/label/Inspiring","indices":[0,22],"display_url":"sarah-jones-larson.blogspot.com/search/label/I\u2026","url":"http://t.co/8ZWlrufdgo"}]}},"url":"http://t.co/8ZWlrufdgo","utc_offset":-18000,"time_zone":"Central Time (US & Canada)","notifications":false,"profile_use_background_image":true,"friends_count":2430,"profile_sidebar_fill_color":"B2FF91","screen_name":"SarahJL","id_str":"14809486","profile_image_url":"http://pbs.twimg.com/profile_images/1023303425/150_color_normal.jpg","listed_count":125,"is_translator":false},"coordinates":null}"""

	// val foo = """{"contributors":null,"text":"@primalpancake @owillis Okay, I'll try it.  I've seen all the L&amp;Os three or four times...","geo":null,"retweeted":false,"in_reply_to_screen_name":"primalpancake","truncated":false,"lang":"en","entities":{"symbols":[],"urls":[],"hashtags":[],"user_mentions":[{"id":260291132,"name":"David","indices":[0,14],"screen_name":"primalpancake","id_str":"260291132"},{"id":3497941,"name":"Oliver Willis","indices":[15,23],"screen_name":"owillis","id_str":"3497941"}]},"in_reply_to_status_id_str":"434107665464045568","id":434107990002515970,"source":"<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone<\/a>","in_reply_to_user_id_str":"260291132","favorited":false,"in_reply_to_status_id":434107665464045568,"retweet_count":0,"created_at":"Thu Feb 13 23:33:04 +0000 2014","in_reply_to_user_id":260291132,"favorite_count":0,"id_str":"434107990002515970","place":null,"user":{"location":"San Francisco","default_profile":true,"profile_background_tile":false,"statuses_count":3856,"lang":"en","profile_link_color":"0084B4","profile_banner_url":"https://pbs.twimg.com/profile_banners/227152034/1380534852","id":227152034,"following":false,"protected":false,"favourites_count":3372,"profile_text_color":"333333","description":"Observer, commenter, Canadian at heart; no retweets, no links, no DMs; love cats, dogs, spiders; Words are too important to be left in the hands of amateurs...","verified":false,"contributors_enabled":false,"profile_sidebar_border_color":"C0DEED","name":"Alberta Lulu Jones","profile_background_color":"C0DEED","created_at":"Thu Dec 16 02:08:45 +0000 2010","is_translation_enabled":false,"default_profile_image":false,"followers_count":566,"profile_image_url_https":"https://pbs.twimg.com/profile_images/1242182585/image_normal.jpg","geo_enabled":false,"profile_background_image_url":"http://abs.twimg.com/images/themes/theme1/bg.png","profile_background_image_url_https":"https://abs.twimg.com/images/themes/theme1/bg.png","follow_request_sent":false,"entities":{"description":{"urls":[]}},"url":null,"utc_offset":null,"time_zone":null,"notifications":false,"profile_use_background_image":true,"friends_count":1234,"profile_sidebar_fill_color":"DDEEF6","screen_name":"410lulu","id_str":"227152034","profile_image_url":"http://pbs.twimg.com/profile_images/1242182585/image_normal.jpg","listed_count":4,"is_translator":false},"coordinates":null}"""

	// val bar = getTextNoStopwords(foo,false,false,false,false)

	// println(bar)
