package com.papauschek.ui

import com.papauschek.puzzle.{Puzzle, PuzzleConfig, PuzzleWords}
import com.papauschek.ui.{Globals, HtmlRenderer}
import org.scalajs.dom
import org.scalajs.dom.Worker
import org.scalajs.dom.{Blob, URL}
import scala.scalajs.js.JSON
import scala.scalajs.js
import scalajs.js.JSConverters._
import org.scalajs.dom.html.{Button, Div, Input, Select, TextArea}
import upickle.default.*
import concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.JSExport

/** the main user interface based on the `index.html` */
class MainPage:

  private var initialPuzzle: Puzzle = Puzzle.empty(PuzzleConfig())
  private var refinedPuzzle: Puzzle = initialPuzzle

  private var mainInputWords: Seq[String] = Nil

  private val inputElement = dom.document.getElementById("input").asInstanceOf[TextArea]
  private val bonusInputElement = dom.document.getElementById("input-bonus-words").asInstanceOf[TextArea]
  private val downloadJsonButton = dom.document.getElementById("download-json-button")

  private val outputPuzzleElement = dom.document.getElementById("output-puzzle")
  private val outputCluesElement = dom.document.getElementById("output-clues")
  private val resultInfoElement = dom.document.getElementById("result-info")

  private val generateButton = dom.document.getElementById("generate-button").asInstanceOf[Button]
  private val generateSpinner = dom.document.getElementById("generate-spinner").asInstanceOf[Div]

  private val widthInputElement = dom.document.getElementById("width").asInstanceOf[Input]
  private val heightInputElement = dom.document.getElementById("height").asInstanceOf[Input]

  private val languageSelect = dom.document.getElementById("language-select").asInstanceOf[Select]

  private val resultRow = dom.document.getElementById("result-row").asInstanceOf[Div]
  private val refineRow = dom.document.getElementById("refine-row").asInstanceOf[Div]
  private val crosswordName = dom.document.getElementById("crossword-name").asInstanceOf[Div]
  private val crosswordNameInput = dom.document.getElementById("crossword-name-input")
  private val cluesRow = dom.document.getElementById("clues-row").asInstanceOf[Div]
  
  generateButton.addEventListener("click", { _ => generateSolution() })
  downloadJsonButton.addEventListener("click", { _ => generateJson() })

  def generateJson(): Unit = {
      val words = inputElement.value.split("\n").map(_.trim).filter(_.nonEmpty).toSeq
      val longestWord = inputElement.value
        .split("\n")            
        .map(_.trim)            
        .filter(_.nonEmpty)     
        .flatMap(_.split("\\s+")) 
        .maxByOption(_.length)  
      val bonusWords = bonusInputElement.value.split("\n").map(_.trim).filter(_.nonEmpty).toSeq
      val annotation = initialPuzzle.getAnnotation

      val positions = annotation.flatMap { case (point, annotatedPoints) =>
        annotatedPoints.map { anno =>
          js.Dynamic.literal(
            "X" -> point.x,
            "Y" -> point.y,
            "Direction" -> (if (anno.vertical) "v" else "h")
          )
        }
      }

      val crosswordNameValue = crosswordNameInput.asInstanceOf[Input].value.trim
      val fileName = if (crosswordNameValue.isEmpty) {
        "crossword.json"
      } else {
        s"crossword $crosswordNameValue.json"
      }
  
      val jsonObject = js.Dynamic.literal(
        "Name" -> crosswordNameValue,
        "Letters" -> longestWord.getOrElse(""),
        "Words" -> words.toJSArray,
        "Position" -> positions.toJSArray,
        "Bonus Words" -> bonusWords.toJSArray
      )
  
      val jsonString = JSON.stringify(jsonObject, space = 2)
  
      downloadJson(jsonString, fileName)
    }

  def downloadJson(content: String, filename: String): Unit = {
    val blob = new dom.Blob(
      js.Array(content),
      new dom.BlobPropertyBag { `type` = "application/json" }
    )

    val url = URL.createObjectURL(blob)
    val a = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
    a.href = url

    a.setAttribute("download", filename)

    a.click()

    URL.revokeObjectURL(url)
  }
    

  /** read the words from the user interface and generate the puzzle in the background using web workers */
  def generateSolution(): Unit =
    val rawInputWords = inputElement.value.linesIterator.map(normalizeWord).toSeq
    val inputWords = rawInputWords.filter(word => word.nonEmpty && !word.startsWith("#"))
    if (inputWords.nonEmpty) {
      mainInputWords = PuzzleWords.sortByBest(inputWords)
      val puzzleConfig = PuzzleConfig(
        width = widthInputElement.valueAsNumber.toInt,
        height = heightInputElement.valueAsNumber.toInt
      )
      generateSpinner.classList.remove("invisible")
      generateButton.classList.add("invisible")

      PuzzleGenerator.send(NewPuzzleMessage(puzzleConfig, mainInputWords)).foreach {
        puzzles =>
          generateSpinner.classList.add("invisible")
          generateButton.classList.remove("invisible")
          resultRow.classList.remove("invisible")
          refineRow.classList.remove("invisible")
          crosswordName.classList.remove("invisible")
          cluesRow.classList.remove("invisible")
          initialPuzzle = puzzles.maxBy(_.density)
          refinedPuzzle = initialPuzzle
          renderSolution()
      }
    }


  /** show the generated puzzle */
  def renderSolution(): Unit =
    val showFullSolution = true

    outputPuzzleElement.innerHTML = HtmlRenderer.renderPuzzle(
      refinedPuzzle,
      showSolution = showFullSolution,
      showPartialSolution = false)

    val unusedWords = mainInputWords.filterNot(refinedPuzzle.words.contains)
    val extraWords = refinedPuzzle.words -- initialPuzzle.words
    resultInfoElement.innerHTML = HtmlRenderer.renderPuzzleInfo(refinedPuzzle, unusedWords)

  /** add words from a chosen dictionary to the puzzle */
  def refineSolution(): Unit =
    val language = languageSelect.value
    val words = Globals.window(language).filter(_.length >= 4)
    refinedPuzzle = Puzzle.finalize(initialPuzzle, words.toList)
    renderSolution()

  /** normalize words and expand german umlauts */
  private def normalizeWord(word: String): String =
    word.trim.toUpperCase.
      replace("Ä", "AE").
      replace("Ö", "OE").
      replace("Ü", "UE").
      replace("ß", "SS")

