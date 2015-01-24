#!/bin/sh
exec scala "$0" "$@"
!#

// How to run: ./indexSongs.scala <song-collection> <lexicon> <stopwords> 
//
// <song-collection> - Xml data containing collections of slovene folk songs
// <lexicon> - Text file containing dictionary of slovene word forms (first 
//				column) and their lemmas (secon column). 
// <stopwords> - Text file containing stopwords, one per line
//
// Will create three new .js files in js subfolder containing json data.
// The first one will be called pesmi.js and will contain data from the
// passed xml file <song-collection>.
// Second one called inverted-index.js will contain inverted index,
// and third one called distances.js will contain cosine similarities
// between song groups.

import java.io.File
import java.io.PrintWriter
import scala.runtime.ScalaRunTime._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.language.postfixOps

object Pesmi {

	var lexicon: Map[String, String] = _
	var stopWords: Set[String] = _

  def main(args: Array[String]) {
		//### 1. LOAD DATA
		println("Loading data...")
		val zbirka: Seq[VarTip] = loadCollection(args.toList(0))   
		lexicon = loadLexicon(args.toList(1))
		stopWords = loadStopWords(args.toList(2))

		//### 2. INDEX 
		println("Normalizing songs...")
		val normalizedSongs: Map[String, Seq[(String, Int)]] = 
			normalizeCollection(zbirka)

		//### 3. INVERTED INDEX 
		println("Creating inverted index...")
		val invertedIndex: Map[String, Map[String, Set[Int]]] =
			createInvertedIndex(normalizedSongs)

		//### 4. VECTOR SPACE MATRIX
		println("Creating bags of words...")
		val bagsOfWords: Map[String, Map[String, Int]] =
			createVectorSpace(normalizedSongs)

		//### 5. COSINUS DISTANCES MATRIX
		println("Calculating cosine similarities...")
		val cosinusDistances: Map[String, Map[String, Double]] =
			calculateCosinusDistances(normalizedSongs, zbirka)

		//### 6. WRITE TO FILES
		println("Writing data to files...")
		writeToFile("js/pesmi.js", songCollectionToJson(zbirka))
		writeToFile("js/inverted-index.js", invertedIndexToJson(invertedIndex))
		writeToFile("js/distances.js", cosinusDistancesToJson(cosinusDistances))
  }

	def writeToFile(fileName: String, text: String) {
		println("Writing to: "+fileName)
		val writer = new PrintWriter(new File(fileName))
		writer.write(text)
		writer.close()
	}


	//###########################
	//##### 1. DATA LOADERS #####
	//###########################

	def loadStopWords(fileName: String): 
															Set[String] = {
		val words = io.Source.fromFile(fileName).mkString.split("\n")
		val trimmedWords = words map { w:String => w.trim }
		trimmedWords.toSet
	}

	def loadLexicon(fileName: String): Map[String, String] = {
		val lexicon: Map[String, String] = Map()
		for(line <- io.Source.fromFile(fileName).getLines()) {
			val words = line.split("[ \t]").toList
			val derivative = words(0).toLowerCase.trim
			val normalForm = words(1).toLowerCase.trim
			lexicon(derivative) = normalForm
		}
		lexicon
	}

  def loadCollection(fileName: String): Seq[VarTip] = {
    val zbirka = scala.xml.XML.loadFile(fileName);

		val varTips = (zbirka \ "varTip").map { varTip =>
			val ime = (varTip \ "ime").text
			val st = (varTip \ "st").text
			val pojasnila = (varTip \ "pojasnila").text
			val pesmi = (varTip \ "pesmi" \ "pesem").map { pesem =>
				val st = (pesem \ "st").text
				// Kraj Zapisa:
				val ime = (pesem \ "krajzapisa" \ "ime").text
				val rezi25id = (pesem \ "krajzapisa" \ "rezi25ID").text
				val rezi25ime = (pesem \ "krajzapisa" \ "rezi25ime").text
				val koordX = (pesem \ "krajzapisa" \ "koordX").text
				val koordY = (pesem \ "krajzapisa" \ "koordY").text
				val krajZapisa = 
					KrajZapisa(ime.trim, rezi25id.trim, rezi25ime.trim, 
											koordX.trim, koordY.trim)
				//
				val zps = (pesem \ "zps").text
				val datum = (pesem \ "datum").text
				val vir = (pesem \ "vir").text
				val obj = (pesem \ "obj").text
				val besedilo = (pesem \ "besedilo").text
				Pesem(st.trim, krajZapisa, zps.trim, datum.trim, 
							vir.trim, obj.trim, besedilo.trim)
			}
			VarTip(ime.trim, st.trim, pojasnila.trim, pesmi)
		}
		varTips
  }


	//#####################
	//##### 2. INDEX ######
	//#####################

	def normalizeCollection(zbirka: Seq[VarTip]): 
																	Map[String, Seq[(String, Int)]] = {
		val normCollection: Map[String, Seq[(String, Int)]] = Map()
		for (varTip <- zbirka) {
			for (pesem <- varTip.pesmi) {
				val combinedId = varTip.st +"-"+ pesem.st
				val normSong = normalizeSong(pesem.besedilo)
				normCollection(combinedId) = normSong
			}
		}
		normCollection
	}

	def normalizeSong(song: String): Seq[(String, Int)] = {
		val words = removePunctuation(song)
		var tokens = addPositions(words)
		tokens = toLowerCase(tokens)
		tokens = removeShortWords(tokens) 		
		tokens = lemmizeWords(tokens) 
		tokens = removeStopWords(tokens)
		tokens
	}	

	def removePunctuation(text: String): Seq[String] =
		("""\b\p{L}+\b""".r findAllIn text).toList

	def addPositions(words: Seq[String]): Seq[(String, Int)] = 
		words.zipWithIndex

	def toLowerCase(words: Seq[(String, Int)]) =
		words map { t => (t._1.toLowerCase.trim, t._2) }

	def removeShortWords(words: Seq[(String, Int)]) =
		words.filter(_._1.length > 2)

	def lemmizeWords(words: Seq[(String, Int)]) =
		words map {	t => ((lexicon getOrElse (t._1, t._1)), t._2) }

	def removeStopWords(words: Seq[(String, Int)]) =
		words.filterNot(t => stopWords.contains(t._1))


	//#############################
	//##### 3. INVERTED INDEX #####
	//#############################

	def createInvertedIndex(normSongs: 
											Map[String, Seq[(String, Int)]]):  
											Map[String, Map[String, Set[Int]]] = {
		val	invIndex: Map[String, Map[String, Set[Int]]] = Map()
		for (idAndSong <- normSongs) {
			val id = idAndSong._1
			val song = idAndSong._2
			for (wordAndLocation <- song) {
				val word = wordAndLocation._1	
				val location = wordAndLocation._2
				if (invIndex contains word) {
					val mapToLocations = invIndex(word)
					addMatch(mapToLocations, id, location)
				} else {
					invIndex(word) = Map((id, Set(location))) 
				}
			}
		}
		invIndex
	}

	def addMatch(mapToLocations: Map[String, Set[Int]], 
							 id: String, location: Int) {
		if (mapToLocations contains id) {
			val locations = mapToLocations(id)
			mapToLocations(id) = locations + location
		} else {
			mapToLocations(id) = Set(location)
		}
	}


	//##############################
	//### 4. VECTOR SPACE MATRIX ###
	//##############################

	def	createVectorSpace(normSongs: Map[String, Seq[(String, Int)]]):
																	 Map[String, Map[String, Int]] = {
		val vectorSpace: Map[String, Map[String, Int]] = Map()
		for (song <- normSongs) {
			val wordCount: Map[String, Int] = Map()
			val id = song._1
			val tokens = song._2 map { t => t._1 }
			updateCounter(wordCount, tokens)
			vectorSpace(id) = wordCount
		}
		vectorSpace
	}	


	//###################################
	//### 5. COSINUS DISTANCES MATRIX ###
	//###################################

	def calculateCosinusDistances(normalizedSongs: Map[String, Seq[(String, Int)]],
																zbirka: Seq[VarTip]):
																Map[String, Map[String, Double]] = {
		val wordsBySong = getWordCountsBySong(normalizedSongs)		
		val orderedWords = getOrderedWords(zbirka)		
		val distances: Map[String, Map[String, Double]] = Map()
		val wordsBySongKeyList = (wordsBySong map { t => t._1 }).toSeq
		for (i <- 0 until wordsBySongKeyList.length-1) {
			for (j <- i+1 until wordsBySongKeyList.length) {
				val key1 = wordsBySongKeyList(i)
				val key2 = wordsBySongKeyList(j)
				val vector1 = fillVector(orderedWords, wordsBySong(key1))
				val vector2 = fillVector(orderedWords, wordsBySong(key2))
				val distance = CosineSimilarity.cosineSimilarity(vector1, vector2)
				val dis = distances.getOrElse(key1,null)
				if (dis == null) {
					val distan: Map[String, Double] = Map()
					distan(key2) = distance
					distances(key1) = distan
				} else {
					distances(key1)(key2) = distance
				}
			}
		}
		distances
	}

	def fillVector(allWords: Seq[String], words: Map[String, Int]): Array[Int] = {
		var vector = new ListBuffer[Int]()
		for (w <- allWords) {
			if (words.contains(w)) {
				vector += words(w)
			} else {
				vector += 0
			}
		}
		vector.toArray
	}

	def getWordCountsBySong(normalizedSongs: Map[String, Seq[(String, Int)]]):
																						Map[String, Map[String, Int]] = {
		val wordCounts: Map[String, Map[String, Int]] =
			createVectorSpace(normalizedSongs)
		val wordCountsBySong: Map[String, Map[String, Int]] = Map()

		for (wordCount <- wordCounts) {
			val secondPart = "-.*".r
			val songsId = secondPart.replaceAllIn(wordCount._1, "")
			wordCountsBySong(songsId) = agregate(wordCountsBySong.getOrElse(songsId, null), wordCount._2)
		}
		wordCountsBySong
	}

	def agregate(counts1: Map[String,Int], counts2: Map[String,Int]) = {
			if (counts1 == null)
				counts2
			else
				counts1 ++ counts2.map{ case (k,v) => k -> (v + counts1.getOrElse(k,0)) }
	}

	def getOrderedWords(zbirka: Seq[VarTip]) = {
		val wordCount: Map[String, Int] =	getWordCount(zbirka)
		(wordCount map { w => w._1 }).toSeq.sorted
	}

	object CosineSimilarity {
		def cosineSimilarity(x: Array[Int], y: Array[Int]): Double = {
			require(x.size == y.size)
			dotProduct(x, y)/(magnitude(x) * magnitude(y))
		}
		def dotProduct(x: Array[Int], y: Array[Int]): Int = {
			(for((a, b) <- x zip y) yield a * b) sum
		}
		def magnitude(x: Array[Int]): Double = {
			math.sqrt(x map(i => i*i) sum)
		}
	}

	//### WORD OCCURANCES COUNTER ###

	def printWordsByOccurances(zbirka: Seq[VarTip]) {
		val wordCount = getWordCount(zbirka) // word -> occurances
		val inverted = wordCount.groupBy(_._2).mapValues(_.map(_._1))
		val sortedKeys = inverted.keySet.toList.sorted
		sortedKeys.foreach(
			i => println(i +": "+ listToString(inverted(i).toList))
		)
	}

	def listToString(list: List[String]): String = {
		val sb = new StringBuilder()
		list.foreach(w => sb.append(w+", "))
		sb.toString.stripSuffix(", ")
	}

	def getWordCount(zbirka: Seq[VarTip]): Map[String, Int] = {
		val wordCount: Map[String, Int] = Map()
		for (varTip <- zbirka) {
			for (pesem <- varTip.pesmi) {
				val words = normalizeSong(pesem.besedilo)
				val onlyWords = words map { tuple => tuple._1 }
				updateCounter(wordCount, onlyWords)
			}
		}
		wordCount
	}

	def updateCounter(wordCount: Map[String, Int], 
										words: Seq[String]) {
		for (word <- words) {
			if (wordCount contains word) {
				wordCount(word) = wordCount(word) + 1
			} else {
				wordCount(word) = 1
			}
		}
	}


	//######################
	//#### 6. TO STRING ####
	//######################

	def songCollectionToJson(songCollection: Seq[VarTip]): String = {
		val sb = new StringBuilder()
		sb ++= "var pesmi = '{\"dokument\": {\n"
		for (varTip <- songCollection) {
			sb ++= varTip.toJson+","
		}
		if (songCollection.size != 0) {
			sb.deleteCharAt(sb.length-1)
		}
		sb ++= "}}';"
		val newLine = "\n".r
		newLine.replaceAllIn(sb.toString, "\\\\\n")
	}

	def invertedIndexToJson(words: Map[String, Map[String, Set[Int]]]):
																	 String = {
		val sb = new StringBuilder()
		sb ++= "var invertedIndex = '{\"words\": {\\\n"
		for (word <- words) {
			sb ++= "\t"+par(word._1)+": ["
			for (id <- word._2) {
				sb ++= "{"+tagValue("id", id._1)+", \"locations\": ["
				for(location <- id._2) {
					sb ++= par(location.toString)+","
				}
				if (id._2.size != 0) {
					sb.deleteCharAt(sb.length-1)
				}
				sb ++= "]},"
			}
			if (word._2.size != 0) {
				sb.deleteCharAt(sb.length-1)
			}
			sb ++= "],\\\n"
		}
		sb.deleteCharAt(sb.length-3)
		sb ++= "}}';"
		sb.toString
	}	

	def cosinusDistancesToJson(cosinusDistances: Map[String, Map[String, Double]]): 
			String = {
		val sb = new StringBuilder()
		sb ++= "var cosinusDistances = '{\"distances\": {\\\n"
		for (distances <- cosinusDistances) {
			sb ++= "\t"+par(distances._1)+": {"
			for (distance <- distances._2) {
				sb ++= tagValue(distance._1, distance._2.toString)+","
			}
			if (distances._2.size != 0) {
				sb.deleteCharAt(sb.length-1)
			}
			sb ++= "},\\\n"
		}
		sb.deleteCharAt(sb.length-3)
		sb ++= "}}';"
		sb.toString
	}

	def normalizedSongsToJson(songs: Map[String, Seq[(String, Int)]]): 
																		 String = {
		val sb = new StringBuilder()
		sb ++= "var normalizedSongs = '{\"songs\": [\\\n"
		for (song <- songs) {
			sb ++= "\t{\"id\": "+par(song._1)+", \"text\": ["
			for (word <- song._2) {
				sb ++= "{"+tagValue("word", word._1)+", "+
								tagValue("location", word._2.toString)+"},"
			}
			if (song._2.length != 0) {
				sb.deleteCharAt(sb.length-1)
			}
			sb ++= "]},\\\n"
		}
		sb.deleteCharAt(sb.length-3)
		sb ++= "]}';"
		sb.toString
	}

	//#### STRING UTIL ####

	def tagValueIfDefined(tag: String, value: String,
												paranthesis: Boolean, comma: Boolean,
												noOfTabs: Int): 
												String = {
		if (value == null || value.length == 0)
			return ""
		val com = if (comma) "," else ""
		val tagAndValue = if (paranthesis) {
			tagValue(tag, value)
		} else {
			tagValueNoPar(tag, value)
		}
		t(noOfTabs)+tagAndValue+com+"\n"
	}

	def tagValueNoPar(tag: String, value: String) =
		par(tag)+": "+value	

	def tagValue(tag: String, value: String) =
		par(tag)+": "+par(value)	

	def par(word: String) = 
		"\""+word+"\""	

	def t(numOfTabs: Int) =
		"\t" * numOfTabs

	// Changes all new lines to '\n',
	// and escapes parenthesis.
	def n(text: String) = {
		var t2 = encodeNewLine(text)
		t2 = encodeTab(t2)
		escapeParenthesis(t2)
	}

	def encodeNewLine(text: String) = {
		val newLine = "\n".r
    newLine.replaceAllIn(text, "\\\\\\\\n")
	}

	def encodeTab(text: String) = {
		val newLine = "\t".r
    newLine.replaceAllIn(text, "\\\\\\\\t")
	}

	def escapeParenthesis(text: String) = {
		val parenthesis = "[\'\"]".r
		parenthesis.replaceAllIn(text, m => "\\\\"+m)
	}


	//######################
	//#### DATA CLASSES ####
	//######################

	case class VarTip(		val ime: 				String, 
												val st: 				String,
												val pojasnila: 	String, 
												var pesmi: 			Seq[Pesem]) {
		def toJson(): String = {
			val sb = new StringBuilder()
			sb ++= t(1)+par(st)+": {\n"
			sb ++= tagValueIfDefined("ime", n(ime), true, true, 2)
			sb ++= tagValueIfDefined("pojasnila", n(pojasnila), 
																		true, true, 2)
			val sbTmp = new StringBuilder() 
			pesmi.foreach(p => sbTmp ++= p.toJson+",\n")
			if (pesmi.size != 0) {
				sbTmp.deleteCharAt(sbTmp.length-2)
			}
			sb ++= tagValueIfDefined("pesmi", "{\n"+sbTmp+"}", 
																		false, false, 2)
			sb ++= t(1)+"}\n"
			sb.toString
		}
	}

	case class Pesem(			val st: 				String,
												val krajZapisa: KrajZapisa,
												val zps: 				String,
												val datum: 			String,
												val vir: 				String,
												val obj: 				String,
												val besedilo: 	String) {
		def toJson(): String = {
			val sb = new StringBuilder()
			sb ++= t(3)+par(st)+": {\n"
			sb ++= tagValueIfDefined("krajzapisa", krajZapisa.toJson, 
																		false, true, 4)
			sb ++= tagValueIfDefined("zps", n(zps), true, true, 4)
			sb ++= tagValueIfDefined("datum", n(datum), true, true, 4)
			sb ++= tagValueIfDefined("vir", n(vir), true, true, 4)
			sb ++= tagValueIfDefined("obj", n(obj), true, true, 4)
			sb ++= tagValueIfDefined("besedilo", n(besedilo), true, true, 4)
			sb.deleteCharAt(sb.length-2)
			sb ++= t(3)+"}\n"
			sb.toString
		}
	}

	case class KrajZapisa(val ime: 				String,
												val rezi25id: 	String,
												val rezi25ime: 	String,
												val koordX: 		String,
												val koordY: 		String) {
		def toJson(): String = {
			val sb = new StringBuilder()
			sb ++= t(4)+"{\n"
			sb ++= tagValueIfDefined("ime", n(ime), true, true, 5)
			sb ++= tagValueIfDefined("rezi25id", n(rezi25id), 
																		true, true, 5)
			sb ++= tagValueIfDefined("rezi25ime", n(rezi25ime), 
																		true, true, 5)
			sb ++= tagValueIfDefined("koordX", n(koordX), true, true, 5)
			sb ++= tagValueIfDefined("koordY", n(koordY), true, false, 5)
			sb ++= t(4)+"}\n"
			sb.toString
		}
	}		

}

