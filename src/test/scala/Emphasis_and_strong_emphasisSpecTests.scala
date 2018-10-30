package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Emphasis_and_strong_emphasisSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 331" in {
    test( "*foo bar*\n" ) shouldBe "<p><em>foo bar</em></p>\n"
  }

  "example 332" in {
    test( "a * foo bar*\n" ) shouldBe "<p>a * foo bar*</p>\n"
  }

  "example 333" in {
    test( "a*\"foo\"*\n" ) shouldBe "<p>a*&quot;foo&quot;*</p>\n"
  }

  "example 334" in {
    test( "* a *\n" ) shouldBe "<p>* a *</p>\n"
  }

  "example 335" in {
    test( "foo*bar*\n" ) shouldBe "<p>foo<em>bar</em></p>\n"
  }

  "example 336" in {
    test( "5*6*78\n" ) shouldBe "<p>5<em>6</em>78</p>\n"
  }

  "example 337" in {
    test( "_foo bar_\n" ) shouldBe "<p><em>foo bar</em></p>\n"
  }

  "example 338" in {
    test( "_ foo bar_\n" ) shouldBe "<p>_ foo bar_</p>\n"
  }

  "example 339" in {
    test( "a_\"foo\"_\n" ) shouldBe "<p>a_&quot;foo&quot;_</p>\n"
  }

  "example 340" in {
    test( "foo_bar_\n" ) shouldBe "<p>foo_bar_</p>\n"
  }

  "example 341" in {
    test( "5_6_78\n" ) shouldBe "<p>5_6_78</p>\n"
  }

  "example 342" in {
    test( "пристаням_стремятся_\n" ) shouldBe "<p>пристаням_стремятся_</p>\n"
  }

  "example 343" in {
    test( "aa_\"bb\"_cc\n" ) shouldBe "<p>aa_&quot;bb&quot;_cc</p>\n"
  }

  "example 344" in {
    test( "foo-_(bar)_\n" ) shouldBe "<p>foo-<em>(bar)</em></p>\n"
  }

  "example 345" in {
    test( "_foo*\n" ) shouldBe "<p>_foo*</p>\n"
  }

  "example 346" in {
    test( "*foo bar *\n" ) shouldBe "<p>*foo bar *</p>\n"
  }

  "example 347" in {
    test( "*foo bar\n*\n" ) shouldBe "<p>*foo bar\n*</p>\n"
  }

  "example 348" in {
    test( "*(*foo)\n" ) shouldBe "<p>*(*foo)</p>\n"
  }

  "example 349" in {
    test( "*(*foo*)*\n" ) shouldBe "<p><em>(<em>foo</em>)</em></p>\n"
  }

  "example 350" in {
    test( "*foo*bar\n" ) shouldBe "<p><em>foo</em>bar</p>\n"
  }

  "example 351" in {
    test( "_foo bar _\n" ) shouldBe "<p>_foo bar _</p>\n"
  }

  "example 352" in {
    test( "_(_foo)\n" ) shouldBe "<p>_(_foo)</p>\n"
  }

  "example 353" in {
    test( "_(_foo_)_\n" ) shouldBe "<p><em>(<em>foo</em>)</em></p>\n"
  }

  "example 354" in {
    test( "_foo_bar\n" ) shouldBe "<p>_foo_bar</p>\n"
  }

  "example 355" in {
    test( "_пристаням_стремятся\n" ) shouldBe "<p>_пристаням_стремятся</p>\n"
  }

  "example 356" in {
    test( "_foo_bar_baz_\n" ) shouldBe "<p><em>foo_bar_baz</em></p>\n"
  }

  "example 357" in {
    test( "_(bar)_.\n" ) shouldBe "<p><em>(bar)</em>.</p>\n"
  }

  "example 358" in {
    test( "**foo bar**\n" ) shouldBe "<p><strong>foo bar</strong></p>\n"
  }

  "example 359" in {
    test( "** foo bar**\n" ) shouldBe "<p>** foo bar**</p>\n"
  }

  "example 360" in {
    test( "a**\"foo\"**\n" ) shouldBe "<p>a**&quot;foo&quot;**</p>\n"
  }

  "example 361" in {
    test( "foo**bar**\n" ) shouldBe "<p>foo<strong>bar</strong></p>\n"
  }

  "example 362" in {
    test( "__foo bar__\n" ) shouldBe "<p><strong>foo bar</strong></p>\n"
  }

  "example 363" in {
    test( "__ foo bar__\n" ) shouldBe "<p>__ foo bar__</p>\n"
  }

  "example 364" in {
    test( "__\nfoo bar__\n" ) shouldBe "<p>__\nfoo bar__</p>\n"
  }

  "example 365" in {
    test( "a__\"foo\"__\n" ) shouldBe "<p>a__&quot;foo&quot;__</p>\n"
  }

  "example 366" in {
    test( "foo__bar__\n" ) shouldBe "<p>foo__bar__</p>\n"
  }

  "example 367" in {
    test( "5__6__78\n" ) shouldBe "<p>5__6__78</p>\n"
  }

  "example 368" in {
    test( "пристаням__стремятся__\n" ) shouldBe "<p>пристаням__стремятся__</p>\n"
  }

  "example 369" in {
    test( "__foo, __bar__, baz__\n" ) shouldBe "<p><strong>foo, <strong>bar</strong>, baz</strong></p>\n"
  }

  "example 370" in {
    test( "foo-__(bar)__\n" ) shouldBe "<p>foo-<strong>(bar)</strong></p>\n"
  }

  "example 371" in {
    test( "**foo bar **\n" ) shouldBe "<p>**foo bar **</p>\n"
  }

  "example 372" in {
    test( "**(**foo)\n" ) shouldBe "<p>**(**foo)</p>\n"
  }

  "example 373" in {
    test( "*(**foo**)*\n" ) shouldBe "<p><em>(<strong>foo</strong>)</em></p>\n"
  }

  "example 374" in {
    test( "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**\n" ) shouldBe "<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.\n<em>Asclepias physocarpa</em>)</strong></p>\n"
  }

  "example 375" in {
    test( "**foo \"*bar*\" foo**\n" ) shouldBe "<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>\n"
  }

  "example 376" in {
    test( "**foo**bar\n" ) shouldBe "<p><strong>foo</strong>bar</p>\n"
  }

  "example 377" in {
    test( "__foo bar __\n" ) shouldBe "<p>__foo bar __</p>\n"
  }

  "example 378" in {
    test( "__(__foo)\n" ) shouldBe "<p>__(__foo)</p>\n"
  }

  "example 379" in {
    test( "_(__foo__)_\n" ) shouldBe "<p><em>(<strong>foo</strong>)</em></p>\n"
  }

  "example 380" in {
    test( "__foo__bar\n" ) shouldBe "<p>__foo__bar</p>\n"
  }

  "example 381" in {
    test( "__пристаням__стремятся\n" ) shouldBe "<p>__пристаням__стремятся</p>\n"
  }

  "example 382" in {
    test( "__foo__bar__baz__\n" ) shouldBe "<p><strong>foo__bar__baz</strong></p>\n"
  }

  "example 383" in {
    test( "__(bar)__.\n" ) shouldBe "<p><strong>(bar)</strong>.</p>\n"
  }

//  "example 384" in {//todo: links
//    test( "*foo [bar](/url)*\n" ) shouldBe "<p><em>foo <a href=\"/url\">bar</a></em></p>\n"
//  }

  "example 385" in {
    test( "*foo\nbar*\n" ) shouldBe "<p><em>foo\nbar</em></p>\n"
  }

  "example 386" in {
    test( "_foo __bar__ baz_\n" ) shouldBe "<p><em>foo <strong>bar</strong> baz</em></p>\n"
  }

  "example 387" in {
    test( "_foo _bar_ baz_\n" ) shouldBe "<p><em>foo <em>bar</em> baz</em></p>\n"
  }

  "example 388" in {
    test( "__foo_ bar_\n" ) shouldBe "<p><em><em>foo</em> bar</em></p>\n"
  }

  "example 389" in {
    test( "*foo *bar**\n" ) shouldBe "<p><em>foo <em>bar</em></em></p>\n"
  }

  "example 390" in {
    test( "*foo **bar** baz*\n" ) shouldBe "<p><em>foo <strong>bar</strong> baz</em></p>\n"
  }

//  "example 391" in {//todo: em
//    test( "*foo**bar**baz*\n" ) shouldBe "<p><em>foo<strong>bar</strong>baz</em></p>\n"
//  }

  "example 392" in {
    test( "***foo** bar*\n" ) shouldBe "<p><em><strong>foo</strong> bar</em></p>\n"
  }

  "example 393" in {
    test( "*foo **bar***\n" ) shouldBe "<p><em>foo <strong>bar</strong></em></p>\n"
  }

//  "example 394" in {//todo: em
//    test( "*foo**bar***\n" ) shouldBe "<p><em>foo<strong>bar</strong></em></p>\n"
//  }

  "example 395" in {
    test( "*foo **bar *baz* bim** bop*\n" ) shouldBe "<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>\n"
  }

//  "example 396" in {//todo: links
//    test( "*foo [*bar*](/url)*\n" ) shouldBe "<p><em>foo <a href=\"/url\"><em>bar</em></a></em></p>\n"
//  }

  "example 397" in {
    test( "** is not an empty emphasis\n" ) shouldBe "<p>** is not an empty emphasis</p>\n"
  }

  "example 398" in {
    test( "**** is not an empty strong emphasis\n" ) shouldBe "<p>**** is not an empty strong emphasis</p>\n"
  }

//  "example 399" in {//todo: links
//    test( "**foo [bar](/url)**\n" ) shouldBe "<p><strong>foo <a href=\"/url\">bar</a></strong></p>\n"
//  }

  "example 400" in {
    test( "**foo\nbar**\n" ) shouldBe "<p><strong>foo\nbar</strong></p>\n"
  }

  "example 401" in {
    test( "__foo _bar_ baz__\n" ) shouldBe "<p><strong>foo <em>bar</em> baz</strong></p>\n"
  }

  "example 402" in {
    test( "__foo __bar__ baz__\n" ) shouldBe "<p><strong>foo <strong>bar</strong> baz</strong></p>\n"
  }

  "example 403" in {
    test( "____foo__ bar__\n" ) shouldBe "<p><strong><strong>foo</strong> bar</strong></p>\n"
  }

  "example 404" in {
    test( "**foo **bar****\n" ) shouldBe "<p><strong>foo <strong>bar</strong></strong></p>\n"
  }

  "example 405" in {
    test( "**foo *bar* baz**\n" ) shouldBe "<p><strong>foo <em>bar</em> baz</strong></p>\n"
  }

//  "example 406" in {//todo: em
//    test( "**foo*bar*baz**\n" ) shouldBe "<p><strong>foo<em>bar</em>baz</strong></p>\n"
//  }

  "example 407" in {
    test( "***foo* bar**\n" ) shouldBe "<p><strong><em>foo</em> bar</strong></p>\n"
  }

  "example 408" in {
    test( "**foo *bar***\n" ) shouldBe "<p><strong>foo <em>bar</em></strong></p>\n"
  }

  "example 409" in {
    test( "**foo *bar **baz**\nbim* bop**\n" ) shouldBe "<p><strong>foo <em>bar <strong>baz</strong>\nbim</em> bop</strong></p>\n"
  }

//  "example 410" in {//todo: links
//    test( "**foo [*bar*](/url)**\n" ) shouldBe "<p><strong>foo <a href=\"/url\"><em>bar</em></a></strong></p>\n"
//  }

  "example 411" in {
    test( "__ is not an empty emphasis\n" ) shouldBe "<p>__ is not an empty emphasis</p>\n"
  }

  "example 412" in {
    test( "____ is not an empty strong emphasis\n" ) shouldBe "<p>____ is not an empty strong emphasis</p>\n"
  }

  "example 413" in {
    test( "foo ***\n" ) shouldBe "<p>foo ***</p>\n"
  }

  "example 414" in {
    test( "foo *\\**\n" ) shouldBe "<p>foo <em>*</em></p>\n"
  }

  "example 415" in {
    test( "foo *_*\n" ) shouldBe "<p>foo <em>_</em></p>\n"
  }

  "example 416" in {
    test( "foo *****\n" ) shouldBe "<p>foo *****</p>\n"
  }

  "example 417" in {
    test( "foo **\\***\n" ) shouldBe "<p>foo <strong>*</strong></p>\n"
  }

  "example 418" in {
    test( "foo **_**\n" ) shouldBe "<p>foo <strong>_</strong></p>\n"
  }

  "example 419" in {
    test( "**foo*\n" ) shouldBe "<p>*<em>foo</em></p>\n"
  }

  "example 420" in {
    test( "*foo**\n" ) shouldBe "<p><em>foo</em>*</p>\n"
  }

  "example 421" in {
    test( "***foo**\n" ) shouldBe "<p>*<strong>foo</strong></p>\n"
  }

  "example 422" in {
    test( "****foo*\n" ) shouldBe "<p>***<em>foo</em></p>\n"
  }

  "example 423" in {
    test( "**foo***\n" ) shouldBe "<p><strong>foo</strong>*</p>\n"
  }

  "example 424" in {
    test( "*foo****\n" ) shouldBe "<p><em>foo</em>***</p>\n"
  }

  "example 425" in {
    test( "foo ___\n" ) shouldBe "<p>foo ___</p>\n"
  }

  "example 426" in {
    test( "foo _\\__\n" ) shouldBe "<p>foo <em>_</em></p>\n"
  }

  "example 427" in {
    test( "foo _*_\n" ) shouldBe "<p>foo <em>*</em></p>\n"
  }

  "example 428" in {
    test( "foo _____\n" ) shouldBe "<p>foo _____</p>\n"
  }

  "example 429" in {
    test( "foo __\\___\n" ) shouldBe "<p>foo <strong>_</strong></p>\n"
  }

  "example 430" in {
    test( "foo __*__\n" ) shouldBe "<p>foo <strong>*</strong></p>\n"
  }

  "example 431" in {
    test( "__foo_\n" ) shouldBe "<p>_<em>foo</em></p>\n"
  }

  "example 432" in {
    test( "_foo__\n" ) shouldBe "<p><em>foo</em>_</p>\n"
  }

  "example 433" in {
    test( "___foo__\n" ) shouldBe "<p>_<strong>foo</strong></p>\n"
  }

  "example 434" in {
    test( "____foo_\n" ) shouldBe "<p>___<em>foo</em></p>\n"
  }

  "example 435" in {
    test( "__foo___\n" ) shouldBe "<p><strong>foo</strong>_</p>\n"
  }

  "example 436" in {
    test( "_foo____\n" ) shouldBe "<p><em>foo</em>___</p>\n"
  }

  "example 437" in {
    test( "**foo**\n" ) shouldBe "<p><strong>foo</strong></p>\n"
  }

  "example 438" in {
    test( "*_foo_*\n" ) shouldBe "<p><em><em>foo</em></em></p>\n"
  }

  "example 439" in {
    test( "__foo__\n" ) shouldBe "<p><strong>foo</strong></p>\n"
  }

  "example 440" in {
    test( "_*foo*_\n" ) shouldBe "<p><em><em>foo</em></em></p>\n"
  }

  "example 441" in {
    test( "****foo****\n" ) shouldBe "<p><strong><strong>foo</strong></strong></p>\n"
  }

  "example 442" in {
    test( "____foo____\n" ) shouldBe "<p><strong><strong>foo</strong></strong></p>\n"
  }

  "example 443" in {
    test( "******foo******\n" ) shouldBe "<p><strong><strong><strong>foo</strong></strong></strong></p>\n"
  }

  "example 444" in {
    test( "***foo***\n" ) shouldBe "<p><em><strong>foo</strong></em></p>\n"
  }

  "example 445" in {
    test( "_____foo_____\n" ) shouldBe "<p><em><strong><strong>foo</strong></strong></em></p>\n"
  }

//  "example 446" in {//todo: em
//    test( "*foo _bar* baz_\n" ) shouldBe "<p><em>foo _bar</em> baz_</p>\n"
//  }

//  "example 447" in {//todo: em
//    test( "*foo __bar *baz bim__ bam*\n" ) shouldBe "<p><em>foo <strong>bar *baz bim</strong> bam</em></p>\n"
//  }

  "example 448" in {
    test( "**foo **bar baz**\n" ) shouldBe "<p>**foo <strong>bar baz</strong></p>\n"
  }

  "example 449" in {
    test( "*foo *bar baz*\n" ) shouldBe "<p>*foo <em>bar baz</em></p>\n"
  }

//  "example 450" in {
//    test( "*[bar*](/url)\n" ) shouldBe "<p>*<a href=\"/url\">bar*</a></p>\n"
//  }
//
//  "example 451" in {
//    test( "_foo [bar_](/url)\n" ) shouldBe "<p>_foo <a href=\"/url\">bar_</a></p>\n"
//  }
//
//  "example 452" in {
//    test( "*<img src=\"foo\" title=\"*\"/>\n" ) shouldBe "<p>*<img src=\"foo\" title=\"*\"/></p>\n"
//  }

  "example 453" in {
    test( "**<a href=\"**\">\n" ) shouldBe "<p>**<a href=\"**\"></p>\n"
  }

  "example 454" in {
    test( "__<a href=\"__\">\n" ) shouldBe "<p>__<a href=\"__\"></p>\n"
  }

  "example 455" in {
    test( "*a `*`*\n" ) shouldBe "<p><em>a <code>*</code></em></p>\n"
  }

  "example 456" in {
    test( "_a `_`_\n" ) shouldBe "<p><em>a <code>_</code></em></p>\n"
  }

  "example 457" in {
    test( "**a<http://foo.bar/?q=**>\n" ) shouldBe "<p>**a<a href=\"http://foo.bar/?q=**\">http://foo.bar/?q=**</a></p>\n"
  }

  "example 458" in {
    test( "__a<http://foo.bar/?q=__>\n" ) shouldBe "<p>__a<a href=\"http://foo.bar/?q=__\">http://foo.bar/?q=__</a></p>\n"
  }

}
