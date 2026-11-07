
let rootHandler req = task {
    panic "qwe"
}

let serve opts endpoints = Zerver.serve opts rootHandler
