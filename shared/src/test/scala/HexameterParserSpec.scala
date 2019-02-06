package edu.holycross.shot.greek
import org.scalatest.FlatSpec




class HexameterParserSpec extends FlatSpec {


  "A hexameter parser" should "return a munged string (no spaces or punctuation)" in {
    val lgs = LiteraryGreekString("οἰωνοῖσί τε πᾶσι, Διὸς δ᾽ ἐτελείετο βουλή,")
    val s1:String = HexameterParser.munge(lgs)
    val expected:String = "oi)wnoisitepasidiosde)teleietoboulh"
    assert (s1 == expected)
  }

  it should "preserve diaeresis when munging" in {
    val lgs = LiteraryGreekString("Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος")
    val s1:String = HexameterParser.munge(lgs)
    val expected:String = "mhnina)eideqeaphlhi+adewa)xilhos"
    assert (s1 == expected)
  }

   it should "expand psi to ps when munging" in {
    val lgs = LiteraryGreekString("πολλὰς δ᾽ ἰφθίμους ψυχὰς Ἄϊδι προΐαψεν")
    val s1:String = HexameterParser.munge(lgs)
    val expected:String = "pollasdi)fqimouspsuxasa)i+diproi+apsen"
    assert (s1 == expected)
  }

  it should "expand xi to xs when munging" in {
    val lgs = LiteraryGreekString("Ἀτρεΐδης τε ἄναξ ἀνδρῶν καὶ δῖος Ἀχιλλεύς.")
    val s1:String = HexameterParser.munge(lgs)
    val expected:String = "a)trei+dhstea)naksa)ndrwnkaidiosa)xilleus"
    assert (s1 == expected)
  }

  // Syllabification
  it should "split a string at a diaeresis" in {
    val lgs = LiteraryGreekString("Ἀτρεΐδης")
    val v1:Vector[LiteraryGreekString] = HexameterParser.splitOnDiaeresis(lgs)
    val expected:Vector[LiteraryGreekString] = Vector(LiteraryGreekString("a)tre"),LiteraryGreekString("i+"), LiteraryGreekString("dhs"))
    assert (v1 == expected)

  }

  it  should "pass a single syllable unchagined but for removal of accents" in {
    val sylls = HexameterParser.syllabify(Vector(LiteraryGreekString("w)=")))
    val syllsAscii = sylls.map(_.ascii)
    assert (syllsAscii == Vector("w)"))
  }

  it should "preserve leading and trailing content when recursively splitting on diaeresis" in {
    val strVector = "Τρῳαὶ ἐϋπλόκαμοι".split(" ").toVector
    val gsVector = strVector.map(LiteraryGreekString(_))
    val sylls = HexameterParser.splitOnDiaeresis(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("τρῳαι","ἐ","ϋ","πλοκαμοι"))
  }


  it should "preserve leading and trailing content when recursively splitting on mn" in {
    val strVector = "τῶν μιμνησκόμενος  ".split(" ").toVector
    val gsVector = strVector.map(LiteraryGreekString(_))
    val sylls = HexameterParser.splitOnMuNu(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("των","μι","μνησκομενοσ"))
  }

   it should "preserve leading and trailing content when recursively splitting on liquid+consonant" in {
    val gsVector = Vector(LiteraryGreekString("σμινθεῦ"))
    val sylls = HexameterParser.splitOnLiqCons(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("σμιν","θευ"))
  }

  it should "preserve leading and trailing content when recursively splitting on diphthong+vowel" in {
    val gsVector = Vector(LiteraryGreekString("βιοῖο"))
    val sylls = HexameterParser.splitOnDiphthVowel(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("βιοι","ο"))
  }

  it should "preserve leading and trailing content when recursively splitting on vowel+diphthong" in {
    val strVector = "ὥς κε νέηαι".split(" ").toVector
    val gsVector = strVector.map(LiteraryGreekString(_))
    val sylls = HexameterParser.splitOnVowelDiphth(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("ὡσ","κε","νεη","αι"))

  }

  it should "correctly syllabify Homeric poetry" in {
    val l1:LiteraryGreekString = LiteraryGreekString("Ἄνδρα μοι ἔννεπε, Μοῦσα, πολύτροπον, ὃς μάλα πολλὰ")
    val l2:LiteraryGreekString = LiteraryGreekString("πλάγχθη, ἐπεὶ Τροίης ἱερὸν πτοkίεθρον ἔπερσε·")
    val l3:LiteraryGreekString = LiteraryGreekString("πολλῶν δ’ ἀνθρώπων ἴδεν ἄστεα καὶ νόον ἔγνω,")
    val l4:LiteraryGreekString = LiteraryGreekString("πολλὰ δ’ ὅ γ’ ἐν πόντῳ πάθεν ἄλγεα ὃν κατὰ θυμόν,")
    val l5:LiteraryGreekString = LiteraryGreekString("ἀρνύμενος ἥν τε ψυχὴν καὶ νόστον ἑταίρων.")
    val l6:LiteraryGreekString = LiteraryGreekString("Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος")
    val l7:LiteraryGreekString = LiteraryGreekString("οὐλομένην, ἣ μυρί' Ἀχαιοῖς ἄλγε' ἔθηκε")

    val iliad1_1_syll:Vector[LiteraryGreekString] = Vector(
      LiteraryGreekString("mh"),
      LiteraryGreekString("ni"),
      LiteraryGreekString("na)"),
      LiteraryGreekString("ei"),
      LiteraryGreekString("de"),
      LiteraryGreekString("qe"),
      LiteraryGreekString("a"),
      LiteraryGreekString("ph"),
      LiteraryGreekString("lh"),
      LiteraryGreekString("i+"),
      LiteraryGreekString("a"),
      LiteraryGreekString("dew"),
      LiteraryGreekString("a)"),
      LiteraryGreekString("xi"),
      LiteraryGreekString("lh"),
      LiteraryGreekString("os")
    ) 
    assert( HexameterParser.syllabify(l6) == iliad1_1_syll )
  }
  

}
