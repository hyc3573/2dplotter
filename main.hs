import Graphics.Implicit

stepperWidth = 42
stepperHeight = 40
wallThickness = 2
frontWallThickness = 10
axisGap = 30
axisDiameter = 8
beltWidth = 8
beltHeight = 10

  
frontWall = rect3 (V3 0 0 0) (V3
                          (stepperHeight * 2 + axisGap)
                          (stepperWidth + wallThickness*2)
                          (frontWallThickness))

frontHole = let hole = cylinder (axisDiameter/2) 20
                beltHole = cube True (V3 beltWidth beltHeight 10) 
            in (V3 (stepperHeight) (stepperWidth/2+wallThickness) 0)
               `translate` union [hole,
                                  translate (V3 axisGap 0 0) hole,
                                  translate (V3 (axisGap/2) 0 0)
                                  beltHole]

front = difference frontWall [frontHole]

sideWall = rect3 (V3
                  (-wallThickness)
                  0
                  (-stepperWidth-wallThickness))
           (V3
             (0)
             (stepperWidth+wallThickness*2)
             (frontWallThickness))


out =
  let length = 600
      stepperWidth = 42
      stepperHeight = 40
      thickness = 2
      axisGap = 30
      axleDia = 8
      beltWidth = 5
      beltHeight = 15
      gap = 1
  in union [front, sideWall]
  
main = writeSTL 2 "test.stl" out
