import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Easy

myLR :: EC (LayoutLR Double Double Double) ()
myLR = do
  let x = [1.9,1.92..18.5]
  layoutlr_title .= "Foo"
  plotLeft (line "cos" [x `zip` (cos `map` x)] )
  plotRight (line "cos" [x `zip` (sin `map` x)] )


main :: IO ()
main = toInteractiveWindow 640 480 myLR

