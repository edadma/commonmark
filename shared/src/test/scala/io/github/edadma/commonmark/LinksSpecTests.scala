package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LinksSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 481" in {
    test( "[link](/uri \"title\")\n" ) shouldBe "<p><a href=\"/uri\" title=\"title\">link</a></p>\n"
  }

  "example 482" in {
    test( "[link](/uri)\n" ) shouldBe "<p><a href=\"/uri\">link</a></p>\n"
  }

  "example 483" in {
    test( "[](./target.md)\n" ) shouldBe "<p><a href=\"./target.md\"></a></p>\n"
  }

  "example 484" in {
    test( "[link]()\n" ) shouldBe "<p><a href=\"\">link</a></p>\n"
  }

  "example 485" in {
    test( "[link](<>)\n" ) shouldBe "<p><a href=\"\">link</a></p>\n"
  }

  "example 486" in {
    test( "[]()\n" ) shouldBe "<p><a href=\"\"></a></p>\n"
  }

  "example 487" in {
    test( "[link](/my uri)\n" ) shouldBe "<p>[link](/my uri)</p>\n"
  }

  "example 488" in {
    test( "[link](</my uri>)\n" ) shouldBe "<p><a href=\"/my%20uri\">link</a></p>\n"
  }

  "example 489" in {
    test( "[link](foo\nbar)\n" ) shouldBe "<p>[link](foo\nbar)</p>\n"
  }

  "example 490" in {
    test( "[link](<foo\nbar>)\n" ) shouldBe "<p>[link](<foo\nbar>)</p>\n"
  }

  "example 491" in {
    test( "[a](<b)c>)\n" ) shouldBe "<p><a href=\"b)c\">a</a></p>\n"
  }

  "example 492" in {
    test( "[link](<foo\\>)\n" ) shouldBe "<p>[link](&lt;foo&gt;)</p>\n"
  }

  "example 493" in {
    test( "[a](<b)c\n[a](<b)c>\n[a](<b>c)\n" ) shouldBe "<p>[a](&lt;b)c\n[a](&lt;b)c&gt;\n[a](<b>c)</p>\n"
  }

  "example 494" in {
    test( "[link](\\(foo\\))\n" ) shouldBe "<p><a href=\"(foo)\">link</a></p>\n"
  }

  "example 495" in {
    test( "[link](foo(and(bar)))\n" ) shouldBe "<p><a href=\"foo(and(bar))\">link</a></p>\n"
  }

  "example 496" in {
    test( "[link](foo(and(bar))\n" ) shouldBe "<p>[link](foo(and(bar))</p>\n"
  }

  "example 497" in {
    test( "[link](foo\\(and\\(bar\\))\n" ) shouldBe "<p><a href=\"foo(and(bar)\">link</a></p>\n"
  }

  "example 498" in {
    test( "[link](<foo(and(bar)>)\n" ) shouldBe "<p><a href=\"foo(and(bar)\">link</a></p>\n"
  }

  "example 499" in {
    test( "[link](foo\\)\\:)\n" ) shouldBe "<p><a href=\"foo):\">link</a></p>\n"
  }

  "example 500" in {
    test( "[link](#fragment)\n\n[link](http://example.com#fragment)\n\n[link](http://example.com?foo=3#frag)\n" ) shouldBe "<p><a href=\"#fragment\">link</a></p>\n<p><a href=\"http://example.com#fragment\">link</a></p>\n<p><a href=\"http://example.com?foo=3#frag\">link</a></p>\n"
  }

  "example 501" in {
    test( "[link](foo\\bar)\n" ) shouldBe "<p><a href=\"foo%5Cbar\">link</a></p>\n"
  }

  "example 502" in {
    test( "[link](foo%20b&auml;)\n" ) shouldBe "<p><a href=\"foo%20b%C3%A4\">link</a></p>\n"
  }

  "example 503" in {
    test( "[link](\"title\")\n" ) shouldBe "<p><a href=\"%22title%22\">link</a></p>\n"
  }

  "example 504" in {
    test( "[link](/url \"title\")\n[link](/url 'title')\n[link](/url (title))\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">link</a>\n<a href=\"/url\" title=\"title\">link</a>\n<a href=\"/url\" title=\"title\">link</a></p>\n"
  }

  "example 505" in {
    test( "[link](/url \"title \\\"&quot;\")\n" ) shouldBe "<p><a href=\"/url\" title=\"title &quot;&quot;\">link</a></p>\n"
  }

  "example 506" in {
    test( "[link](/url \"title\")\n" ) shouldBe "<p><a href=\"/url%C2%A0%22title%22\">link</a></p>\n"
  }

  "example 507" in {
    test( "[link](/url \"title \"and\" title\")\n" ) shouldBe "<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>\n"
  }

  "example 508" in {
    test( "[link](/url 'title \"and\" title')\n" ) shouldBe "<p><a href=\"/url\" title=\"title &quot;and&quot; title\">link</a></p>\n"
  }

  "example 509" in {
    test( "[link](   /uri\n  \"title\"  )\n" ) shouldBe "<p><a href=\"/uri\" title=\"title\">link</a></p>\n"
  }

  "example 510" in {
    test( "[link] (/uri)\n" ) shouldBe "<p>[link] (/uri)</p>\n"
  }

  "example 511" in {
    test( "[link [foo [bar]]](/uri)\n" ) shouldBe "<p><a href=\"/uri\">link [foo [bar]]</a></p>\n"
  }

  "example 512" in {
    test( "[link] bar](/uri)\n" ) shouldBe "<p>[link] bar](/uri)</p>\n"
  }

  "example 513" in {
    test( "[link [bar](/uri)\n" ) shouldBe "<p>[link <a href=\"/uri\">bar</a></p>\n"
  }

  "example 514" in {
    test( "[link \\[bar](/uri)\n" ) shouldBe "<p><a href=\"/uri\">link [bar</a></p>\n"
  }

  "example 515" in {
    test( "[link *foo **bar** `#`*](/uri)\n" ) shouldBe "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
  }

  "example 516" in {
    test( "[![moon](moon.jpg)](/uri)\n" ) shouldBe "<p><a href=\"/uri\"><img src=\"moon.jpg\" alt=\"moon\" /></a></p>\n"
  }

  "example 517" in {
    test( "[foo [bar](/uri)](/uri)\n" ) shouldBe "<p>[foo <a href=\"/uri\">bar</a>](/uri)</p>\n"
  }

  "example 518" in {
    test( "[foo *[bar [baz](/uri)](/uri)*](/uri)\n" ) shouldBe "<p>[foo <em>[bar <a href=\"/uri\">baz</a>](/uri)</em>](/uri)</p>\n"
  }

  "example 519" in {
    test( "![[[foo](uri1)](uri2)](uri3)\n" ) shouldBe "<p><img src=\"uri3\" alt=\"[foo](uri2)\" /></p>\n"
  }

  "example 520" in {
    test( "*[foo*](/uri)\n" ) shouldBe "<p>*<a href=\"/uri\">foo*</a></p>\n"
  }

  "example 521" in {
    test( "[foo *bar](baz*)\n" ) shouldBe "<p><a href=\"baz*\">foo *bar</a></p>\n"
  }

  "example 522" in {
    test( "*foo [bar* baz]\n" ) shouldBe "<p><em>foo [bar</em> baz]</p>\n"
  }

  "example 523" in {
    test( "[foo <bar attr=\"](baz)\">\n" ) shouldBe "<p>[foo <bar attr=\"](baz)\"></p>\n"
  }

  "example 524" in {
    test( "[foo`](/uri)`\n" ) shouldBe "<p>[foo<code>](/uri)</code></p>\n"
  }

  "example 525" in {
    test( "[foo<http://example.com/?search=](uri)>\n" ) shouldBe "<p>[foo<a href=\"http://example.com/?search=%5D(uri)\">http://example.com/?search=](uri)</a></p>\n"
  }

  "example 526" in {
    test( "[foo][bar]\n\n[bar]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }

  "example 527" in {
    test( "[link [foo [bar]]][ref]\n\n[ref]: /uri\n" ) shouldBe "<p><a href=\"/uri\">link [foo [bar]]</a></p>\n"
  }

  "example 528" in {
    test( "[link \\[bar][ref]\n\n[ref]: /uri\n" ) shouldBe "<p><a href=\"/uri\">link [bar</a></p>\n"
  }

  "example 529" in {
    test( "[link *foo **bar** `#`*][ref]\n\n[ref]: /uri\n" ) shouldBe "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
  }

  "example 530" in {
    test( "[![moon](moon.jpg)][ref]\n\n[ref]: /uri\n" ) shouldBe "<p><a href=\"/uri\"><img src=\"moon.jpg\" alt=\"moon\" /></a></p>\n"
  }

  "example 531" in {
    test( "[foo [bar](/uri)][ref]\n\n[ref]: /uri\n" ) shouldBe "<p>[foo <a href=\"/uri\">bar</a>]<a href=\"/uri\">ref</a></p>\n"
  }

  "example 532" in {
    test( "[foo *bar [baz][ref]*][ref]\n\n[ref]: /uri\n" ) shouldBe "<p>[foo <em>bar <a href=\"/uri\">baz</a></em>]<a href=\"/uri\">ref</a></p>\n"
  }

  "example 533" in {
    test( "*[foo*][ref]\n\n[ref]: /uri\n" ) shouldBe "<p>*<a href=\"/uri\">foo*</a></p>\n"
  }

  "example 534" in {
    test( "[foo *bar][ref]*\n\n[ref]: /uri\n" ) shouldBe "<p><a href=\"/uri\">foo *bar</a>*</p>\n"
  }

  "example 535" in {
    test( "[foo <bar attr=\"][ref]\">\n\n[ref]: /uri\n" ) shouldBe "<p>[foo <bar attr=\"][ref]\"></p>\n"
  }

  "example 536" in {
    test( "[foo`][ref]`\n\n[ref]: /uri\n" ) shouldBe "<p>[foo<code>][ref]</code></p>\n"
  }

  "example 537" in {
    test( "[foo<http://example.com/?search=][ref]>\n\n[ref]: /uri\n" ) shouldBe "<p>[foo<a href=\"http://example.com/?search=%5D%5Bref%5D\">http://example.com/?search=][ref]</a></p>\n"
  }

  "example 538" in {
    test( "[foo][BaR]\n\n[bar]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }

  "example 539" in {
    test( "[ẞ]\n\n[SS]: /url\n" ) shouldBe "<p><a href=\"/url\">ẞ</a></p>\n"
  }

  "example 540" in {
    test( "[Foo\n  bar]: /url\n\n[Baz][Foo bar]\n" ) shouldBe "<p><a href=\"/url\">Baz</a></p>\n"
  }

  "example 541" in {
    test( "[foo] [bar]\n\n[bar]: /url \"title\"\n" ) shouldBe "<p>[foo] <a href=\"/url\" title=\"title\">bar</a></p>\n"
  }

  "example 542" in {
    test( "[foo]\n[bar]\n\n[bar]: /url \"title\"\n" ) shouldBe "<p>[foo]\n<a href=\"/url\" title=\"title\">bar</a></p>\n"
  }

  "example 543" in {
    test( "[foo]: /url1\n\n[foo]: /url2\n\n[bar][foo]\n" ) shouldBe "<p><a href=\"/url1\">bar</a></p>\n"
  }

  "example 544" in {
    test( "[bar][foo\\!]\n\n[foo!]: /url\n" ) shouldBe "<p>[bar][foo!]</p>\n"
  }

  "example 545" in {
    test( "[foo][ref[]\n\n[ref[]: /uri\n" ) shouldBe "<p>[foo][ref[]</p>\n<p>[ref[]: /uri</p>\n"
  }

  "example 546" in {
    test( "[foo][ref[bar]]\n\n[ref[bar]]: /uri\n" ) shouldBe "<p>[foo][ref[bar]]</p>\n<p>[ref[bar]]: /uri</p>\n"
  }

  "example 547" in {
    test( "[[[foo]]]\n\n[[[foo]]]: /url\n" ) shouldBe "<p>[[[foo]]]</p>\n<p>[[[foo]]]: /url</p>\n"
  }

  "example 548" in {
    test( "[foo][ref\\[]\n\n[ref\\[]: /uri\n" ) shouldBe "<p><a href=\"/uri\">foo</a></p>\n"
  }

  "example 549" in {
    test( "[bar\\\\]: /uri\n\n[bar\\\\]\n" ) shouldBe "<p><a href=\"/uri\">bar\\</a></p>\n"
  }

  "example 550" in {
    test( "[]\n\n[]: /uri\n" ) shouldBe "<p>[]</p>\n<p>[]: /uri</p>\n"
  }

  "example 551" in {
    test( "[\n ]\n\n[\n ]: /uri\n" ) shouldBe "<p>[\n]</p>\n<p>[\n]: /uri</p>\n"
  }

  "example 552" in {
    test( "[foo][]\n\n[foo]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }

  "example 553" in {
    test( "[*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\"><em>foo</em> bar</a></p>\n"
  }

  "example 554" in {
    test( "[Foo][]\n\n[foo]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">Foo</a></p>\n"
  }

  "example 555" in {
    test( "[foo] \n[]\n\n[foo]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">foo</a>\n[]</p>\n"
  }

  "example 556" in {
    test( "[foo]\n\n[foo]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }

  "example 557" in {
    test( "[*foo* bar]\n\n[*foo* bar]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\"><em>foo</em> bar</a></p>\n"
  }

  "example 558" in {
    test( "[[*foo* bar]]\n\n[*foo* bar]: /url \"title\"\n" ) shouldBe "<p>[<a href=\"/url\" title=\"title\"><em>foo</em> bar</a>]</p>\n"
  }

  "example 559" in {
    test( "[[bar [foo]\n\n[foo]: /url\n" ) shouldBe "<p>[[bar <a href=\"/url\">foo</a></p>\n"
  }

  "example 560" in {
    test( "[Foo]\n\n[foo]: /url \"title\"\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">Foo</a></p>\n"
  }

  "example 561" in {
    test( "[foo] bar\n\n[foo]: /url\n" ) shouldBe "<p><a href=\"/url\">foo</a> bar</p>\n"
  }

  "example 562" in {
    test( "\\[foo]\n\n[foo]: /url \"title\"\n" ) shouldBe "<p>[foo]</p>\n"
  }

  "example 563" in {
    test( "[foo*]: /url\n\n*[foo*]\n" ) shouldBe "<p>*<a href=\"/url\">foo*</a></p>\n"
  }

  "example 564" in {
    test( "[foo][bar]\n\n[foo]: /url1\n[bar]: /url2\n" ) shouldBe "<p><a href=\"/url2\">foo</a></p>\n"
  }

  "example 565" in {
    test( "[foo][]\n\n[foo]: /url1\n" ) shouldBe "<p><a href=\"/url1\">foo</a></p>\n"
  }

  "example 566" in {
    test( "[foo]()\n\n[foo]: /url1\n" ) shouldBe "<p><a href=\"\">foo</a></p>\n"
  }

  "example 567" in {
    test( "[foo](not a link)\n\n[foo]: /url1\n" ) shouldBe "<p><a href=\"/url1\">foo</a>(not a link)</p>\n"
  }

  "example 568" in {
    test( "[foo][bar][baz]\n\n[baz]: /url\n" ) shouldBe "<p>[foo]<a href=\"/url\">bar</a></p>\n"
  }

  "example 569" in {
    test( "[foo][bar][baz]\n\n[baz]: /url1\n[bar]: /url2\n" ) shouldBe "<p><a href=\"/url2\">foo</a><a href=\"/url1\">baz</a></p>\n"
  }

  "example 570" in {
    test( "[foo][bar][baz]\n\n[baz]: /url1\n[foo]: /url2\n" ) shouldBe "<p>[foo]<a href=\"/url1\">bar</a></p>\n"
  }

}
