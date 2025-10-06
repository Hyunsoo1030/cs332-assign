package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   * how often the character appears.
   * This list is sorted alphabetically w.r.t. to the character in each pair.
   * All characters in the occurrence list are lowercase.
   * * Any list of pairs of lowercase characters and their frequency which is not sorted
   * is **not** an occurrence list.
   * * Note: If the frequency of some character is zero, then that character should not be
   * in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   * * Note: the uppercase and lowercase version of the character are treated as the
   * same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    w.toLowerCase
      .groupBy(char => char)
      .mapValues(_.length)
      .toList
      .sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.mkString)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   * the words that have that occurrence count.
   * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy(wordOccurrences)
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())
  }

  /** Returns the list of all subsets of the occurrence list.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    occurrences match {
      case Nil => List(Nil)
      case (char, count) :: rest =>
        val combiRest = combinations(rest)

        val currentCombi: List[Occurrences] =
          (for {
            i <- 0 to count
            subCombi <- combiRest
          } yield {
            if (i == 0) subCombi
            else (char, i) :: subCombi
          }).toList

        currentCombi
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = x.toMap
    val yMap = y.toMap

    val result: Map[Char, Int] = yMap.foldLeft(xMap) {
      case (accMap, (char, count)) =>
        val newCount = accMap.getOrElse(char, 0) - count
        accMap.updated(char, newCount)
    }

    result.filter(_._2 > 0).toList.sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val initialOccurrences = sentenceOccurrences(sentence)

    def solve(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) {
        List(Nil)
      } else {
        for {
          subsetOccurrences <- combinations(occurrences)
          word <- dictionaryByOccurrences.getOrElse(subsetOccurrences, Nil)
          remainingOccurrences = subtract(occurrences, subsetOccurrences)
          remainingSentence <- solve(remainingOccurrences)
        } yield {
          word :: remainingSentence
        }
      }
    }

    solve(initialOccurrences)
  }
}