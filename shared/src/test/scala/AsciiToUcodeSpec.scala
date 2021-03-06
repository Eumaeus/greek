package edu.holycross.shot.greek
import org.scalatest.FlatSpec




class AsciiToUcodeSpec extends FlatSpec {




  "A literary Greek string's ascii to unicode transcoding" should "map single ASCII characters to a codepoint" in {

    assert(LiteraryGreekString.asciiToUcode("m","") == "μ")
  }

  it should "map sequences of consonants to an equal number of codepoints" in {
    assert(LiteraryGreekString.asciiToUcode("tr","") == "τρ")
  }

  it should "map sequences of vowels to an equal number of codepoints" in {
    assert(LiteraryGreekString.asciiToUcode("ei","") == "ει")
  }

  it should "map vowel+breathing+vowel to two code points" in {
    assert(LiteraryGreekString.asciiToUcode("a" + ')' + "a","") == "ἀα")
  }

  it should "map vowel+breathing+accent to one code point" in {
      assert(LiteraryGreekString.asciiToUcode("a" + ')' + "/","") == "ἄ")
  }

  it should "happily accept iota subscript as a vowel" in {
    assert (LiteraryGreekString.asciiToUcode("dw/rw|","") == "δώρῳ")
  }

  it should "accept white space" in {
    val expected = "μῆνιν ἄειδε θεὰ"
    val submitted = "mh=nin a)/eide qea\\"
    assert (LiteraryGreekString.asciiToUcode(submitted,"") == expected)
  }

  it should "construct single-accented vowel in ancient Greek range" in {

    val rightAlpha = "μάλα"
    val  wrongAlpha = "μάλα"

    val s1 = LiteraryGreekString("ma/la")
    val s2 = LiteraryGreekString(rightAlpha)

    assert(s1.ascii == s2.ascii)
    assert (s1 == s2)

    val s3 = LiteraryGreekString(wrongAlpha)
    // This only works in JVM:
    //   assert (s1 == s3)
    // This happens in ScalaJS:
    //assert(s3.ascii.contains("#"))

  }

  it should "correctly convert Unicode terminal sigma" in  pending /*{
    val wrath = LiteraryGreekString("μῆνις ")
    val wrathish = LiteraryGreekString("μῆνισ ")
    assert (wrathish.ascii == "mh=nis ")
    assert (wrath.ascii == "mh=nis ")
  }*/

  it should "map to terminal sigma when sigma followed by space" in pending /*{
    val wrath = LiteraryGreekString("mh=nis ")
    val wrath2 =  LiteraryGreekString("μῆνις ")
    assert (wrath == wrath2)

  }*/



}
