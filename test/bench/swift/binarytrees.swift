// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Swift adaptation of binary-trees Go #8,
// that in turn is based on Rust #4.
// Used DispatchQueue.concurrentPerform() to launch the
// worker threads.
//
// contributed by Marcel Ibes

import Dispatch
import Foundation

indirect enum Tree {
    case Empty
    case Node(left: Tree, right: Tree)
}

func itemCheck(_ tree: Tree) -> UInt32 {
    switch tree {
    case .Node(let left, let right):
        switch (left, right) {
        case (.Empty, .Empty):
            return 1
        default:
            return 1 + itemCheck(left) + itemCheck(right)
        }
    case .Empty:
        return 1
    }
}

func bottomUpTree(_ depth: UInt32) -> Tree {
    if depth > 0 {
        return .Node(left: bottomUpTree(depth - 1),
                     right: bottomUpTree(depth - 1))
    }

    return .Node(left: .Empty, right: .Empty)
}

func inner(depth: UInt32, iterations: UInt32) -> String {
    var chk = UInt32(0)
    for _ in 0..<iterations {
        let a = bottomUpTree(depth)
        chk += itemCheck(a)
    }

    return "\(iterations)\t trees of depth \(depth)\t check: \(chk)"
}

let n: UInt32

if CommandLine.argc > 1 {
    n = UInt32(CommandLine.arguments[1]) ?? UInt32(21)
} else {
    n = 21
}

let minDepth = UInt32(4)
let maxDepth = (n > minDepth + 2) ? n : minDepth + 2
var messages: [UInt32: String] = [:]
let depth = maxDepth + 1

let group = DispatchGroup()

let workerQueue = DispatchQueue(label: "workerQueue", qos: .userInteractive, attributes: .concurrent)
let messageQueue = DispatchQueue(label: "messageQueue", qos: .background)

group.enter()
workerQueue.async {
    let tree = bottomUpTree(depth)

    messageQueue.async {
        messages[0] = "stretch tree of depth \(depth)\t check: \(itemCheck(tree))"
        group.leave()
    }
}

group.enter()
workerQueue.async {
    let longLivedTree = bottomUpTree(maxDepth)

    messageQueue.async {
        messages[UINT32_MAX] = "long lived tree of depth \(maxDepth)\t check: \(itemCheck(longLivedTree))"
        group.leave()
    }
}

let halfDepth = (minDepth / 2)
let halfMaxDepth = (maxDepth / 2 + 1)
let itt = Int(halfMaxDepth - halfDepth)

DispatchQueue.concurrentPerform(iterations: itt, execute: { idx in
    let depth = (halfDepth + UInt32(idx)) * 2
    let iterations = UInt32(1 << (maxDepth - depth + minDepth))

    group.enter()
    workerQueue.async {
        let msg = inner(depth: depth, iterations: iterations)
        messageQueue.async {
            messages[depth] = msg
            group.leave()
        }
    }
})

// Wait for all the operations to finish
group.wait()

for msg in messages.sorted(by: { $0.0 < $1.0 }) {
    print(msg.value)
}
