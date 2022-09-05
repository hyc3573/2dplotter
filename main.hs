import Graphics.Implicit

out =
  let length = 600
      stepperWidth = 42
      stepperHeight = 40
      thickness = 2
      axisGap = 30
      axleDia = 8
   in union
        [ rect3 (V3 0 0 0) (V3 10 (stepperWidth + thickness * 2) (- stepperWidth)),
          difference
            ( rect3
                (V3 0 0 0)
                ( V3
                    (stepperHeight * 2 + axisGap)
                    (stepperWidth + thickness * 2)
                    thickness
                )
            )
            [ translate
                ( V3
                    (stepperHeight)
                    (stepperWidth / 2 + thickness)
                    (-10)
                )
                ( cylinder (axleDia / 2) 50
                ),
              translate
                ( V3
                    (stepperHeight + axisGap)
                    (stepperWidth / 2 + thickness)
                    (-10)
                )
                (cylinder (axleDia / 2) 50)
            ]
        ]

main = writeSTL 2 "test.stl" out
