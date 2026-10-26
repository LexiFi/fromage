let { x; _ } = r
let { x; _; } = r
let { x; y; _ } = r
let { x : t; _ } = r
let { x = a; _ } = r
let { M.x; _ } = r
let { x (* c *) ; (* c *) _ (* c *) ; (* c *) } = r
let f = function { x; _ } | { x = _; _ } -> x
