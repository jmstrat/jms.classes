context("Convert expressions")

subscript=bquote(Text[sub])
superscript=bquote(Text^1)
superscript2=bquote(Text^MoreText)
space=bquote(Text~MoreText)
concat=bquote(Text*MoreText)
superExpr=expression("Text"^-1)

test_that("HTML conversion gives correct result", {
  expect_equal(expressionToHTML(subscript),"Text<sub>sub</sub>")
  expect_equal(expressionToHTML(superscript),"Text<sup>1</sup>")
  expect_equal(expressionToHTML(superscript2),"Text<sup>MoreText</sup>")
  expect_equal(expressionToHTML(space),"Text MoreText")
  expect_equal(expressionToHTML(concat),"TextMoreText")
  expect_equal(expressionToHTML(superExpr),"Text<sup>-1</sup>")
})

test_that("String conversion gives correct result", {
  expect_equal(expressionToString(subscript),'Textsub')
  expect_equal(expressionToString(superscript),'Text1')
  expect_equal(expressionToString(superscript2),'TextMoreText')
  expect_equal(expressionToString(space),"Text MoreText")
  expect_equal(expressionToString(concat),"TextMoreText")
  expect_equal(expressionToString(superExpr),"Text-1")
})
