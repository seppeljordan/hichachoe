module Output (Renderable (render) )
where

class Renderable r where
    render :: r -> IO ()
