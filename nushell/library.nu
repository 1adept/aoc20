
export def mkPath [day:int, test:bool] {
    let d = ($day | fill -a r -c '0' -w 2) 
    let type = if ($test) { "example" } else { "input" }

    $"../data/($d)_($type).in"
}