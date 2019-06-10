context("Convert expressions")

subscript=bquote(Text[sub])
superscript=bquote(Text^1)
superscript2=bquote(Text^MoreText)
space=bquote(Text~MoreText)
concat=bquote(Text*MoreText)
superExpr=expression("Text"^-1)
deltaExpr=bquote(delta~"("^"23"*"Na"*")"~"/"~ppm)
cumExpr=expression("Cumulative Capacity / mAhg"^-1)

test_that("HTML conversion gives correct result", {
  expect_equal(expressionToHTML(subscript),htmltools::HTML("Text<sub>sub</sub>"))
  expect_equal(expressionToHTML(superscript),htmltools::HTML("Text<sup>1</sup>"))
  expect_equal(expressionToHTML(superscript2),htmltools::HTML("Text<sup>MoreText</sup>"))
  expect_equal(expressionToHTML(space),htmltools::HTML("Text MoreText"))
  expect_equal(expressionToHTML(concat),htmltools::HTML("TextMoreText"))
  expect_equal(expressionToHTML(superExpr),htmltools::HTML("Text<sup>-1</sup>"))
  expect_equal(expressionToHTML(deltaExpr),htmltools::HTML("&#948 (<sup>23</sup>Na) / ppm"))
  expect_equal(expressionToHTML(cumExpr),htmltools::HTML("Cumulative Capacity / mAhg<sup>-1</sup>"))  # Cumu != Cu <mu> !!
})

test_that("String conversion gives correct result", {
  expect_equal(expressionToString(subscript),'Textsub')
  expect_equal(expressionToString(superscript),'Text1')
  expect_equal(expressionToString(superscript2),'TextMoreText')
  expect_equal(expressionToString(space),"Text MoreText")
  expect_equal(expressionToString(concat),"TextMoreText")
  expect_equal(expressionToString(superExpr),"Text-1")
})
