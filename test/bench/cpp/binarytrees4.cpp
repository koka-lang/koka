/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Danial Klimkin
 *
 */

#include <functional>
#include <iostream>
#include <memory_resource>
#include <mutex>
#include <queue>
#include <thread>

using MemoryPool = std::pmr::monotonic_buffer_resource;
using Lambda = std::function<void(void)>;

template<typename T>
class LockingQueue final {
public:
  LockingQueue() = default;

  void emplace(T item) {
    {
      std::scoped_lock lock(mutex);
      queue.emplace(std::move(item));
    }
    signal.notify_one();
  }

  void waitAndPop(T *out) {
    std::unique_lock lock(mutex);
    while (queue.empty()) {
      signal.wait(lock);
    }
    *out = queue.front();
    queue.pop();
  }

private:
  std::mutex mutex;
  std::condition_variable signal;
  std::queue<T> queue;
};

class ThreadPool final {
public:
  explicit ThreadPool(
      const unsigned int workers = std::thread::hardware_concurrency()) {
    threads_.reserve(workers);
    for (size_t i = 0; i < workers; ++i)
      threads_.emplace_back(std::thread([this]() {
        Lambda job;
        while (true) {
          queue_.waitAndPop(&job);
          if (job) {
            job();
          } else {
            enqueue_task(nullptr);
            break;
          }
        }
      }));
  }

  ~ThreadPool() noexcept {
    enqueue_task(nullptr);
    for (std::thread &thread : threads_) {
      thread.join();
    }
  }

  void enqueue_task(Lambda f) {
    queue_.emplace(std::move(f));
  }

private:
  std::vector<std::thread> threads_;
  LockingQueue<Lambda> queue_;
};

struct Node {
  Node *l, *r;

  int check() const {
    if (l)
      return l->check() + 1 + r->check();
    else
      return 1;
  }
};

namespace {
  constexpr size_t LINE_SIZE = 64;
  constexpr size_t SIZEOF_NODE = sizeof(Node);
  constexpr auto MIN_DEPTH = 4;
}

Node *make(const int d, MemoryPool &store) {
  Node *root = static_cast<Node *>(store.allocate(SIZEOF_NODE));
  if (d > 0) {
    root->l = make(d - 1, store);
    root->r = make(d - 1, store);
  } else {
    root->l = root->r = nullptr;
  }
  return root;
}

int main(int argc, char *argv[]) {
  const int max_depth =
      std::max(MIN_DEPTH + 2, (argc == 2 ? atoi(argv[1]) : 21));
  const int stretch_depth = max_depth + 1;

  // Alloc then dealloc stretchdepth tree.
  {
    MemoryPool store;

    Node *c = make(stretch_depth, store);
    std::cout << "stretch tree of depth " << stretch_depth << "\t "
              << "check: " << c->check() << std::endl;
  }

  MemoryPool long_lived_store;
  Node *long_lived_tree = make(max_depth, long_lived_store);

  // Buffer to store output of each thread.
  std::vector<char> buf(
      static_cast<size_t>(LINE_SIZE * (max_depth + 1) * sizeof(char)), 0);

  {
    ThreadPool pool;
    for (int d = MIN_DEPTH; d <= max_depth; d += 2) {
      const int iterations = 1 << (max_depth - d + MIN_DEPTH);
      pool.enqueue_task([iterations, d, &buf]() {
        int c = 0;

        // Create a memory pool for this thread to use.
        MemoryPool store;

        for (int i = 1; i <= iterations; ++i) {
          Node *a = make(d, store);
          c += a->check();
          store.release();
        }

        // each thread write to separate location
        sprintf(buf.data() + LINE_SIZE * d,
                "%d\t trees of depth %d\t check: %d\n",
                iterations, d, c);
      });
    }
  }

  // print all results
  for (int d = MIN_DEPTH; d <= max_depth; d += 2) {
    printf("%s", buf.data() + (d * LINE_SIZE));
  }

  std::cout << "long lived tree of depth " << max_depth << "\t "
            << "check: " << (long_lived_tree->check()) << "\n";

  return 0;
}
