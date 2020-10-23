let add = fn(x, y) { x + y }

let sub = fn(x, y) { x - y }

let mul = fn(x, y) { x * y }

let div = fn(x, y) { x / y }

let map = fn(f, arr) {
  let iter = fn(arr, acc) {
    if (
        len(arr) == 0) {
      acc
    } else {
      iter(tail(arr), push(acc, f(head(arr))));
    }
  };
  iter(arr, []);
};

let reduce = fn(f, initial, arr) {
  let iter = fn(arr, res) {
    if (len(arr) == 0) {
      res
    } else {
      iter(tail(arr), f(res, head(arr)));
    }
  };
  iter(arr, initial);
};

let join = fn(sep, arr) {
  let iter = fn(arr, sep, res) {
    if (len(arr) == 0) {
      res
    } else {
      iter(tail(arr), sep, sprint(res, sep, head(arr)));
    }
  }
  if (len(arr) == 0) {
    ""
  } else {
    iter(tail(arr), sep, head(arr))
  }
}

let slowMod = fn(b, val) {
  let iter = fn(b, val) {
    if (val < b) { 
      val
    } else {
      iter(b, val - b)
    }
  }
  if (val > -1) {
    iter(b, val)
  }
}

let filter = fn(cond, arr) {
  let iter = fn(arr, acc) {
    if (len(arr) == 0) {
      acc
    } else {
      if (cond(head(arr))) {
        iter(tail(arr), push(acc, head(arr)))
      } else {
        iter(tail(arr), acc)
      }
    }
  };
  iter(arr, []);
}

let range = fn(start, end) {
  let iter = fn(start, end, res) {
    if (start == end) {
      res
    } else {
      iter(start + 1, end, push(res, start))
    }
  }
  iter(start, end, []);
}

let fold = fn(op, arr) {
  let iter = fn(arr, op, acc) {
    if(len(arr) == 0) {
      acc
    } else {
      iter(tail(arr), op, op(acc, head(arr)))
    }
  }
  if (len(arr) == 0) {
    return
  } else {
    iter(tail(arr), op, head(arr))
  }
}

let wfold = fn(op, arr) {
  if (len(arr) != 0) {  
    let acc = head(arr)
    let arr = tail(arr)
    while {
      if (len(arr) == 0) {
        return acc
      }
      print(acc)
      let acc = op(acc, head(arr)) 
      let arr = tail(arr)
    }
  }
}
