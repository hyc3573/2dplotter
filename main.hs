import Graphics.Implicit

stepperWidth = 42
stepperHeight = 40
wallThickness = 2
frontWallThickness = 10
axisGap = 30
axisDiameter = 8
beltWidth = 7
beltHeight = 17
sideWalGap = 35
pullyHoleDiameter = 5

  
frontWall = rect3 (V3 0 0 0) (V3
                          (stepperHeight * 2 + axisGap)
                          (stepperWidth + wallThickness*2)
                          (frontWallThickness))

frontHole = let hole = cylinder (axisDiameter/2) 20
                beltHole = cube True (V3 beltWidth beltHeight (frontWallThickness*2)) 
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


stepperSide = union [front, sideWall]
noStepperSide = let wall1 = translate (V3 (stepperHeight-axisDiameter/2) 0 0) sideWall
                    wall2 = translate (V3 (stepperHeight+axisGap+axisDiameter/2+wallThickness) 0 0) sideWall
                    hole =
                      translate (V3 0 (stepperWidth/2+wallThickness) (-stepperWidth/2-wallThickness)) .
                      rotate3V (pi/2) (V3 0 1 0) $
                      cylinder (pullyHoleDiameter/2) (stepperHeight*3)
                in union [front, union [wall1, wall2] `difference` [hole]]
  
main = do writeSTL 2 "stepperside.stl" stepperSide
          writeSTL 2 "nostepperside.stl" noStepperSide
