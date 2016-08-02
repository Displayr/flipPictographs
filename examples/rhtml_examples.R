# Check that the underlying rhtmlPictographs package is working properly with some simple examples

# No recoloring
a1 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":5,"numRows":1,"variableImage":"url:fromleft:https://dl.dropboxusercontent.com/u/539177224/star_filled.svg","baseImage":"url:https://dl.dropboxusercontent.com/u/539177224/star_unfilled.svg","width":250,"height":50,"background-color":"transparent","resizable":"false"}')

# When variable image is recolored, the base image disappears
a2 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":5,"numRows":1,"variableImage":"url:fromleft:red:https://dl.dropboxusercontent.com/u/539177224/star_filled.svg","baseImage":"url:https://dl.dropboxusercontent.com/u/539177224/star_unfilled.svg","width":250,"height":50,"background-color":"transparent","resizable":"false"}')

a3 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":5,"numRows":1,"variableImage":"url:fromleft:red:https://dl.dropboxusercontent.com/u/539177224/star_filled.svg","baseImage":"url:red:https://dl.dropboxusercontent.com/u/539177224/star_unfilled.svg","width":250,"height":50,"background-color":"transparent","resizable":"false"}')

a4 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":5,"numRows":1,"variableImage":"url:fromleft:red:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","baseImage":"url:grey:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","width":250,"height":50,"background-color":"transparent","resizable":"false"}')

a0 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":0,"numRows":1,"variableImage":"url:fromleft:red:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","baseImage":"url:grey:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","width":250,"height":50,"background-color":"transparent","resizable":"false"}')

# Check adding text
b1 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":5,"numRows":1,"variableImage":"url:fromleft:black:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","baseImage":"url:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","width":250,"height":70,"text-footer":{"text":"a", "font-size":"10px","font-weight":"bold", "horizontal-align":"center"},"background-color":"transparent","resizable":"false"}')

b2 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":5,"numRows":1,"variableImage":"url:fromleft:black:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","baseImage":"url:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","width":250,"height":70,"text-overlay":{"text":"a", "font-size":"10px","font-weight":"bold", "horizontal-align":"center"},"background-color":"transparent","resizable":"false"}')

# Check default layout
c1 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":20, "variableImage":"url:fromleft:black:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","baseImage":"url:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","width":250,"height":70,"background-color":"transparent","resizable":"false"}')
c2 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":50, "variableImage":"url:fromleft:black:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","baseImage":"url:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","width":250,"height":70,"background-color":"transparent","resizable":"false"}')
c3 <- rhtmlPictographs::graphic('{"proportion":0.5,"numImages":90, "variableImage":"url:fromleft:black:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","baseImage":"url:https://dl.dropboxusercontent.com/u/539177224/star_grey.svg","width":250,"height":70,"background-color":"transparent","resizable":"true"}')
