package info.sumito3478.text

import java.lang.{ Character => JChar }

/**
 * Represents Unicode Code Point.
 *
 * @param intern 32-bit integer that represents Unicode Code Point
 */
class CodePoint(val intern: Int) extends AnyVal {
  def toChars: IndexedSeq[Char] = {
    JChar.toChars(intern)
  }

  def digit(radix: Int): Int = {
    JChar.digit(intern, radix)
  }

  def name: String = {
    JChar.getName(intern)
  }

  def toNumeric(): Int = {
    JChar.getNumericValue(intern)
  }

  // def getType(): Int = ...

  def isAlphabetic(): Boolean = {
    JChar.isAlphabetic(intern)
  }

  def isBmpCodePoint(): Boolean = {
    JChar.isBmpCodePoint(intern)
  }

  def isDefined(): Boolean = {
    JChar.isDefined(intern)
  }

  def isDigit(): Boolean = {
    JChar.isDigit(intern)
  }

  def isIdentifierIgnorable(): Boolean = {
    JChar.isIdentifierIgnorable(intern)
  }

  def isIdeographic(): Boolean = {
    JChar.isIdeographic(intern)
  }

  def isISOControl(): Boolean = {
    JChar.isISOControl(intern)
  }

  def isJavaIdentifierPart(): Boolean = {
    JChar.isJavaIdentifierPart(intern)
  }

  def isLetter(): Boolean = {
    JChar.isLetter(intern)
  }

  def isLetterOrDigit(): Boolean = {
    JChar.isLetterOrDigit(intern)
  }

  def isLowerCase(): Boolean = {
    JChar.isLowerCase(intern)
  }

  def isMirrored(): Boolean = {
    JChar.isMirrored(intern)
  }

  def isSpaceChar(): Boolean = {
    JChar.isSpaceChar(intern)
  }

  def isSupplementaryCodePoint(): Boolean = {
    JChar.isSupplementaryCodePoint(intern)
  }

  def isTitleCase(): Boolean = {
    JChar.isTitleCase(intern)
  }

  def isUnicodeIdentifierPart(): Boolean = {
    JChar.isUnicodeIdentifierPart(intern)
  }

  def isUpperCase(): Boolean = {
    JChar.isUpperCase(intern)
  }

  def isValidCodePoint(): Boolean = {
    JChar.isValidCodePoint(intern)
  }

  def isWhiteSpace(): Boolean = {
    JChar.isWhitespace(intern)
  }

  def toTitleCase(): CodePoint = {
    new CodePoint(JChar.toTitleCase(intern))
  }

  // def toUpperCase ...
  // def toLowerCase ...
}

object CodePoint {
  implicit def fromInt(value: Int): CodePoint = {
    new CodePoint(value)
  }

  implicit def toInt(codePoint: CodePoint): Int = {
    codePoint.intern
  }

  def isSurrogatePair(high: Char, low: Char): Boolean = {
    JChar.isSurrogatePair(high, low)
  }

  def apply(high: Char, low: Char): CodePoint = {
    JChar.toCodePoint(high, low)
  }

  def apply(digit: Int, radix: Int): CodePoint = {
    JChar.forDigit(digit, radix)
  }
}
