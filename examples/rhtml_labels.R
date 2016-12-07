# Check that labels are handled properly by rhtmlPictographs

t1 <- rhtmlPictographs::graphic('{
  "proportion": "=3/5",
  "numImages": 5,
  "numRows": 1,
  "variableImage": "circle:fromleft:lightblue",
  "floatingLabels": [
    {
      "position": "0:3",
      "text": "percentage"
    }
  ]
}')

t2 <- rhtmlPictographs::graphic('{
  "numImages": 6,
  "numRows": 2,
  "variableImage": "circle:lightblue",
  "floatingLabels": [
    {
      "position": "0:0",
      "font-size": "10px",
      "text": "0:0"
    },
    {
      "position": "0:1",
      "font-size": "10px",
      "text": "0:1"
    },
    {
      "position": "0:2",
      "font-size": "10px",
      "text": "0:2"
    },
    {
      "position": "1:0",
      "font-size": "10px",
      "text": "1:0"
    },
    {
      "position": "1:1",
      "font-size": "10px",
      "text": "1:1"
    },
    {
      "position": "1:2",
      "font-size": "10px",
      "text": "1:2"
    }
  ]
}')

