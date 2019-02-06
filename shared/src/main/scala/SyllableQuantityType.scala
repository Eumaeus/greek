package edu.holycross.shot.greek

/** Sealed trait for explicit enumeration of metrical quantities
*/
sealed abstract trait SyllableQuantityType

/** Type of all long syllables*/
case object LongSyllableType extends SyllableQuantityType

/** Type of all long syllables*/
case object ShortSyllableType extends SyllableQuantityType

/** Type of all long syllables*/
case object AncepsSyllableType extends SyllableQuantityType
