package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class HTML_blocksSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

//  "example 116" in {//todo: em
//    test( "<table><tr><td>\n<pre>\n**Hello**,\n\n_world_.\n</pre>\n</td></tr></table>\n" ) shouldBe "<table><tr><td>\n<pre>\n**Hello**,\n<p><em>world</em>.\n</pre></p>\n</td></tr></table>\n"
//  }

  "example 117" in {
    test( "<table>\n  <tr>\n    <td>\n           hi\n    </td>\n  </tr>\n</table>\n\nokay.\n" ) shouldBe "<table>\n  <tr>\n    <td>\n           hi\n    </td>\n  </tr>\n</table>\n<p>okay.</p>\n"
  }

  "example 118" in {
    test( " <div>\n  *hello*\n         <foo><a>\n" ) shouldBe " <div>\n  *hello*\n         <foo><a>\n"
  }

  "example 119" in {
    test( "</div>\n*foo*\n" ) shouldBe "</div>\n*foo*\n"
  }

  "example 120" in {
    test( "<DIV CLASS=\"foo\">\n\n*Markdown*\n\n</DIV>\n" ) shouldBe "<DIV CLASS=\"foo\">\n<p><em>Markdown</em></p>\n</DIV>\n"
  }

  "example 121" in {
    test( "<div id=\"foo\"\n  class=\"bar\">\n</div>\n" ) shouldBe "<div id=\"foo\"\n  class=\"bar\">\n</div>\n"
  }

  "example 122" in {
    test( "<div id=\"foo\" class=\"bar\n  baz\">\n</div>\n" ) shouldBe "<div id=\"foo\" class=\"bar\n  baz\">\n</div>\n"
  }

  "example 123" in {
    test( "<div>\n*foo*\n\n*bar*\n" ) shouldBe "<div>\n*foo*\n<p><em>bar</em></p>\n"
  }

  "example 124" in {
    test( "<div id=\"foo\"\n*hi*\n" ) shouldBe "<div id=\"foo\"\n*hi*\n"
  }

  "example 125" in {
    test( "<div class\nfoo\n" ) shouldBe "<div class\nfoo\n"
  }

  "example 126" in {
    test( "<div *???-&&&-<---\n*foo*\n" ) shouldBe "<div *???-&&&-<---\n*foo*\n"
  }

  "example 127" in {
    test( "<div><a href=\"bar\">*foo*</a></div>\n" ) shouldBe "<div><a href=\"bar\">*foo*</a></div>\n"
  }

  "example 128" in {
    test( "<table><tr><td>\nfoo\n</td></tr></table>\n" ) shouldBe "<table><tr><td>\nfoo\n</td></tr></table>\n"
  }

  "example 129" in {
    test( "<div></div>\n``` c\nint x = 33;\n```\n" ) shouldBe "<div></div>\n``` c\nint x = 33;\n```\n"
  }

  "example 130" in {
    test( "<a href=\"foo\">\n*bar*\n</a>\n" ) shouldBe "<a href=\"foo\">\n*bar*\n</a>\n"
  }

  "example 131" in {
    test( "<Warning>\n*bar*\n</Warning>\n" ) shouldBe "<Warning>\n*bar*\n</Warning>\n"
  }

  "example 132" in {
    test( "<i class=\"foo\">\n*bar*\n</i>\n" ) shouldBe "<i class=\"foo\">\n*bar*\n</i>\n"
  }

  "example 133" in {
    test( "</ins>\n*bar*\n" ) shouldBe "</ins>\n*bar*\n"
  }

  "example 134" in {
    test( "<del>\n*foo*\n</del>\n" ) shouldBe "<del>\n*foo*\n</del>\n"
  }

  "example 135" in {
    test( "<del>\n\n*foo*\n\n</del>\n" ) shouldBe "<del>\n<p><em>foo</em></p>\n</del>\n"
  }

//  "example 136" in {//todo: em
//    test( "<del>*foo*</del>\n" ) shouldBe "<p><del><em>foo</em></del></p>\n"
//  }

//  "example 137" in {//todo: html block
//    test( "<pre language=\"haskell\"><code>\nimport Text.HTML.TagSoup\n\nmain :: IO ()\nmain = print $ parseTags tags\n</code></pre>\nokay\n" ) shouldBe "<pre language=\"haskell\"><code>\nimport Text.HTML.TagSoup\n\nmain :: IO ()\nmain = print $ parseTags tags\n</code></pre>\n<p>okay</p>\n"
//  }

//  "example 138" in {//todo: html block
//    test( "<script type=\"text/javascript\">\n// JavaScript example\n\ndocument.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n</script>\nokay\n" ) shouldBe "<script type=\"text/javascript\">\n// JavaScript example\n\ndocument.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n</script>\n<p>okay</p>\n"
//  }

//  "example 139" in {//todo: html block
//    test( "<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\nokay\n" ) shouldBe "<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\n<p>okay</p>\n"
//  }

//  "example 140" in {//todo: html block
//    test( "<style\n  type=\"text/css\">\n\nfoo\n" ) shouldBe "<style\n  type=\"text/css\">\n\nfoo\n"
//  }

  "example 141" in {
    test( "> <div>\n> foo\n\nbar\n" ) shouldBe "<blockquote>\n<div>\nfoo\n</blockquote>\n<p>bar</p>\n"
  }

  "example 142" in {
    test( "- <div>\n- foo\n" ) shouldBe "<ul>\n<li>\n<div>\n</li>\n<li>foo</li>\n</ul>\n"
  }

//  "example 143" in {//todo: em
//    test( "<style>p{color:red;}</style>\n*foo*\n" ) shouldBe "<style>p{color:red;}</style>\n<p><em>foo</em></p>\n"
//  }

  "example 144" in {
    test( "<!-- foo -->*bar*\n*baz*\n" ) shouldBe "<!-- foo -->*bar*\n<p><em>baz</em></p>\n"
  }

  "example 145" in {
    test( "<script>\nfoo\n</script>1. *bar*\n" ) shouldBe "<script>\nfoo\n</script>1. *bar*\n"
  }

  "example 146" in {
    test( "<!-- Foo\n\nbar\n   baz -->\nokay\n" ) shouldBe "<!-- Foo\n\nbar\n   baz -->\n<p>okay</p>\n"
  }

  "example 147" in {
    test( "<?php\n\n  echo '>';\n\n?>\nokay\n" ) shouldBe "<?php\n\n  echo '>';\n\n?>\n<p>okay</p>\n"
  }

  "example 148" in {
    test( "<!DOCTYPE html>\n" ) shouldBe "<!DOCTYPE html>\n"
  }

  "example 149" in {
    test( "<![CDATA[\nfunction matchwo(a,b)\n{\n  if (a < b && a < 0) then {\n    return 1;\n\n  } else {\n\n    return 0;\n  }\n}\n]]>\nokay\n" ) shouldBe "<![CDATA[\nfunction matchwo(a,b)\n{\n  if (a < b && a < 0) then {\n    return 1;\n\n  } else {\n\n    return 0;\n  }\n}\n]]>\n<p>okay</p>\n"
  }

  "example 150" in {
    test( "  <!-- foo -->\n\n    <!-- foo -->\n" ) shouldBe "  <!-- foo -->\n<pre><code>&lt;!-- foo --&gt;\n</code></pre>\n"
  }

  "example 151" in {
    test( "  <div>\n\n    <div>\n" ) shouldBe "  <div>\n<pre><code>&lt;div&gt;\n</code></pre>\n"
  }

  "example 152" in {
    test( "Foo\n<div>\nbar\n</div>\n" ) shouldBe "<p>Foo</p>\n<div>\nbar\n</div>\n"
  }

  "example 153" in {
    test( "<div>\nbar\n</div>\n*foo*\n" ) shouldBe "<div>\nbar\n</div>\n*foo*\n"
  }

//  "example 154" in {//todo: can't interrupt a paragraph, but should still be treated as raw text
//    test( "Foo\n<a href=\"bar\">\nbaz\n" ) shouldBe "<p>Foo\n<a href=\"bar\">\nbaz</p>\n"
//  }

//  "example 155" in {
//    test( "<div>\n\n*Emphasized* text.\n\n</div>\n" ) shouldBe "<div>\n<p><em>Emphasized</em> text.</p>\n</div>\n"
//  }

  "example 156" in {
    test( "<div>\n*Emphasized* text.\n</div>\n" ) shouldBe "<div>\n*Emphasized* text.\n</div>\n"
  }

  "example 157" in {
    test( "<table>\n\n<tr>\n\n<td>\nHi\n</td>\n\n</tr>\n\n</table>\n" ) shouldBe "<table>\n<tr>\n<td>\nHi\n</td>\n</tr>\n</table>\n"
  }

  "example 158" in {
    test( "<table>\n\n  <tr>\n\n    <td>\n      Hi\n    </td>\n\n  </tr>\n\n</table>\n" ) shouldBe "<table>\n  <tr>\n<pre><code>&lt;td&gt;\n  Hi\n&lt;/td&gt;\n</code></pre>\n  </tr>\n</table>\n"
  }

}
