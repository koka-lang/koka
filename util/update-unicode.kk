import std/os/path
import std/os/dir
import std/os/file
import std/os/process
import std/os/env
import std/os/flags
import std/core/undiv

struct iflags
  asian-wide: bool = False
  derived-core-properties: bool = False

val header = "usage:\n stack exec koka -- -e util/update-unicode [-- [options]]\n\noptions:"

val flag-descs = 
  fun set-asian-wide( f : iflags, b : bool ) : iflags { f(asian-wide = b) }
  fun set-derived-core-properties( f : iflags, b : bool ) : iflags { f(derived-core-properties = b) }
  [ Flag( "a", ["asian-wide"], Bool(set-asian-wide), "print updated asian wide information" ), 
    Flag( "d", ["derived-core-properties"], Bool(set-derived-core-properties), "print updated derived core property information")]

pub fun process-flags() : <ndet,console> maybe<iflags>
  val (flags,args,errs) = parse( Iflags(), flag-descs, get-args() )
  if errs.is-nil && args.is-nil then Just(flags) else
    println( errs.join("\n") ++ "\n" ++ flag-descs.usage(header) )
    Nothing

type range
  Range(bottom: string, top: string, description: string)

fun hex-ranges(file: path): <exn,fsys> list<range>
  val lines = file.read-text-file().split("\n")
  lines.filter-map() fn(line)
    val values = line.split(";")
    if values.length >= 2 then
      val range = values[0].unjust.split("..")
      val info = values.drop(1).join(";").trim().split("#")
      if info.length == 2 then
        if range.length == 2 then
          val desc = info[0].unjust.trim()
          val bottom = range[0].unjust.trim()
          val top = range[1].unjust.trim()
          Just(Range(bottom, top, desc))
        elif range.length == 1 then
          val desc = info[0].unjust.trim()
          val bottom = range[0].unjust.trim()
          Just(Range(bottom, bottom, desc))
        else
          throw("Err unsupported range")
      else
        Nothing
    else 
      Nothing


fun filter-desc-eq(ranges: list<range>, desc: string): list<range>
  ranges.filter(fn(r) desc == r.description)

fun filter-desc(ranges: list<range>, pred: (string) -> bool): list<range>
  ranges.filter(fn(r) pred(r.description))

fun batch(l: list<a>, n: int): list<list<a>>
  fun recur(l1 : list<a>, accl : list<a>, acc: list<list<a>>, left : int): list<list<a>>
    match l1
      Nil -> 
        if accl.length == 0 then acc.reverse
        else Cons(accl, acc).reverse
      Cons(x, rst) ->
        if left == 1 then
          recur(rst, [], Cons(Cons(x, accl).reverse, acc), n)
        else
          recur(rst, Cons(x, accl), acc, left - 1)
  recur(l, [], [], n)

fun property-ranges(doc: string, koka-name: string, ranges: list<range>)
  "// Doc from unicode data file".println
  doc.split("\n").foreach fn(s)
    ("// " ++ s).println
  println("val " ++ koka-name ++ r" : delayed<total,rtree> = delay{
  build-rtree([")
  ranges.batch(8).foreach fn(rangex)
    println("    " ++ rangex.map(show-range).join(",") ++ ",")
  println("  ])\n}")

fun show-range(r: range)
  if r.bottom == r.top then "point(0x" ++ r.bottom ++ ")"
  else "single(0x" ++ r.bottom ++ ",0x"  ++ r.top ++ ")"

fun main()
  val flags = process-flags().unjust
  if flags.asian-wide then 
    val file = run-system(r#"curl "https://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt" > EastAsianWidth.txt"#)
    if "EastAsianWidth.txt".path.is-file then 
      val ranges = hex-ranges("EastAsianWidth.txt".path)
      property-ranges(r#"East_Asian_Width property, consisting of one of the following values:
       "A", "F", "H", "N", "Na", "W"
- All code points, assigned or unassigned, that are not listed
    explicitly are given the value "N".
- The unassigned code points in the following blocks default to "W":
       CJK Unified Ideographs Extension A: U+3400..U+4DBF
       CJK Unified Ideographs:             U+4E00..U+9FFF
       CJK Compatibility Ideographs:       U+F900..U+FAFF
- All undesignated code points in Planes 2 and 3, whether inside or
    outside of allocated blocks, default to "W":
       Plane 2:                            U+20000..U+2FFFD
       Plane 3: "#, "asian-wide", ranges.filter-desc-eq("W"))
    else
      println("Could not find EastAsianWidth.txt\n\tPlease download from https://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt to your current directory") 
  elif flags.derived-core-properties then
    val file = run-system(r#"curl "https://www.unicode.org/Public/UNIDATA/DerivedCoreProperties.txt" > DerivedCoreProperties.txt"#)
    if "DerivedCoreProperties.txt".path.is-file then 
      val ranges = hex-ranges("DerivedCoreProperties.txt".path)
      property-ranges(r"
Derived Property: Math
 Generated from: Sm + Other_Math", "math", ranges.filter-desc-eq("Math"))
      property-ranges(r"Derived Property: Alphabetic
 Generated from: Uppercase + Lowercase + Lt + Lm + Lo + Nl + Other_Alphabetic", "alphabetic", ranges.filter-desc-eq("Alphabetic"))
      property-ranges(r"Derived Property: Lowercase
 Generated from: Ll + Other_Lowercase", "lower", ranges.filter-desc-eq("Lowercase"))
      property-ranges(r"Derived Property: Uppercase
 Generated from: Lu + Other_Uppercase", "upper", ranges.filter-desc-eq("Uppercase"))
      property-ranges(r"Derived Property:   Cased (Cased)
 As defined by Unicode Standard Definition D135
 C has the Lowercase or Uppercase property or has a General_Category value of Titlecase_Letter.", "cased", ranges.filter-desc-eq("Cased"))
      property-ranges(r"Derived Property:   Case_Ignorable (CI)
 As defined by Unicode Standard Definition D136
 C is defined to be case-ignorable if
   Word_Break(C) = MidLetter or MidNumLet or Single_Quote, or
   General_Category(C) = Nonspacing_Mark (Mn), Enclosing_Mark (Me), Format (Cf), Modifier_Letter (Lm), or Modifier_Symbol (Sk).",
        "case-ignore", ranges.filter-desc-eq("Case_Ignorable"))
      property-ranges(r#"Derived Property:   Changes_When_Lowercased (CWL)
 Characters whose normalized forms are not stable under a toLowercase mapping.
 For more information, see D139 in Section 3.13, "Default Case Algorithms".
 Changes_When_Lowercased(X) is true when toLowercase(toNFD(X)) != toNFD(X)"#,
        "lowercase-changes", ranges.filter-desc-eq("Changes_When_Lowercased"))
      property-ranges(r#"Derived Property:   Changes_When_Uppercased (CWU)
 Characters whose normalized forms are not stable under a toUppercase mapping.
 For more information, see D140 in Section 3.13, "Default Case Algorithms".
 Changes_When_Uppercased(X) is true when toUppercase(toNFD(X)) != toNFD(X)"#,
        "uppercase-changes", ranges.filter-desc-eq("Changes_When_Uppercased"))
      property-ranges(r#"Derived Property:   Changes_When_Titlecased (CWT)
 Characters whose normalized forms are not stable under a toTitlecase mapping.
 For more information, see D141 in Section 3.13, "Default Case Algorithms".
 Changes_When_Titlecased(X) is true when toTitlecase(toNFD(X)) != toNFD(X)"#,
        "titlecase-changes", ranges.filter-desc-eq("Changes_When_Titlecased"))
      property-ranges(r#"Derived Property:   Changes_When_Casefolded (CWCF)
 Characters whose normalized forms are not stable under case folding.
 For more information, see D142 in Section 3.13, "Default Case Algorithms".
 Changes_When_Casefolded(X) is true when toCasefold(toNFD(X)) != toNFD(X)"#,
        "casefolded-changes", ranges.filter-desc-eq("Changes_When_Casefolded"))
      property-ranges(r#"Derived Property:   Changes_When_Casemapped (CWCM)
 Characters whose normalized forms are not stable under case mapping.
 For more information, see D143 in Section 3.13, "Default Case Algorithms".
 Changes_When_Casemapped(X) is true when CWL(X), or CWT(X), or CWU(X)"#,
        "casemapped-changes", ranges.filter-desc-eq("Changes_When_Casemapped"))
      property-ranges(r#"Derived Property: ID_Start
 Characters that can start an identifier.
 Generated from:
     Lu + Ll + Lt + Lm + Lo + Nl
   + Other_ID_Start
   - Pattern_Syntax
   - Pattern_White_Space
 NOTE: See UAX #31 for more information"#,
        "id-start", ranges.filter-desc-eq("ID_Start"))
      property-ranges(r#"Derived Property: ID_Continue
 Characters that can continue an identifier.
 Generated from:
     ID_Start
   + Mn + Mc + Nd + Pc
   + Other_ID_Continue
   - Pattern_Syntax
   - Pattern_White_Space
 NOTE: See UAX #31 for more information"#,
        "id-continue", ranges.filter-desc-eq("ID_Continue"))
      property-ranges(r#"Derived Property: XID_Start
 ID_Start modified for closure under NFKx
 Modified as described in UAX #15
 NOTE: Does NOT remove the non-NFKx characters.
       Merely ensures that if isIdentifer(string) then isIdentifier(NFKx(string))
 NOTE: See UAX #31 for more information"#,
        "xid-start", ranges.filter-desc-eq("XID_Start"))
      property-ranges(r#"Derived Property: XID_Continue
 Mod_ID_Continue modified for closure under NFKx
 Modified as described in UAX #15
 NOTE: Does NOT remove the non-NFKx characters.
       Merely ensures that if isIdentifer(string) then isIdentifier(NFKx(string))
 NOTE: See UAX #31 for more information"#,
        "xid-continue", ranges.filter-desc-eq("XID_Continue"))
      property-ranges(r#"Derived Property: Default_Ignorable_Code_Point
 Generated from
   Other_Default_Ignorable_Code_Point
 + Cf (Format characters)
 + Variation_Selector
 - White_Space
 - FFF9..FFFB (Interlinear annotation format characters)
 - 13430..13440 (Egyptian hieroglyph format characters)
 - Prepended_Concatenation_Mark (Exceptional format characters that should be visible)
#
There are currently no stability guarantees for DICP. However, the
values of DICP interact with the derivation of XID_Continue
and NFKC_CF, for which there are stability guarantees.
Maintainers of this property should note that in the
unlikely case that the DICP value changes for an existing character
which is also XID_Continue=Yes, then exceptions must be put
in place to ensure that the NFKC_CF mapping value for that
existing character does not change."#,
        "default-ignorable", ranges.filter-desc-eq("Default_Ignorable_Code_Point"))
      
      property-ranges(r#"Derived Property: Grapheme_Extend
 Generated from: Me + Mn + Other_Grapheme_Extend
 Note: depending on an application's interpretation of Co (private use),
 they may be either in Grapheme_Base, or in Grapheme_Extend, or in neither."#,
        "grapheme-extend", ranges.filter-desc-eq("Grapheme_Extend"))
      property-ranges(r#"Derived Property: Grapheme_Base
 Generated from: [0..10FFFF] - Cc - Cf - Cs - Co - Cn - Zl - Zp - Grapheme_Extend
 Note: depending on an application's interpretation of Co (private use),
 they may be either in Grapheme_Base, or in Grapheme_Extend, or in neither."#,
        "grapheme-base", ranges.filter-desc-eq("Grapheme_Base"))
      property-ranges(r#"Derived Property: Grapheme_Link (deprecated)
 Generated from: Canonical_Combining_Class=Virama
 Use Canonical_Combining_Class=Virama directly instead"#,
        "grapheme-link", ranges.filter-desc-eq("Grapheme_Link"))
      property-ranges(r#"Derived Property: Indic_Conjunct_Break
 Generated from the Grapheme_Cluster_Break, Indic_Syllabic_Category,
 Canonical_Combining_Class, and Script properties as described in UAX #44:
 https://www.unicode.org/reports/tr44/.

 All code points not explicitly listed for Indic_Conjunct_Break
 have the value None.

@missing: 0000..10FFFF; InCB; None

================================================

Indic_Conjunct_Break=Linker"#,
        "indic-break-linker", ranges.filter-desc-eq("InCB; Linker"))
      property-ranges(r#"Indic_Conjunct_Break=Consonant"#,
        "indic-break-consonant", ranges.filter-desc-eq("InCB; Consonant"))
      property-ranges(r#"Indic_Conjunct_Break=Extend"#,
        "indic-break-extend", ranges.filter-desc-eq("InCB; Extend"))
    else
      println("Could not find DerivedCoreProperties.txt\n\tPlease download from https://www.unicode.org/Public/UNIDATA/DerivedCoreProperties.txt to your current directory") 