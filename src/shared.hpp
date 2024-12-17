#ifndef SHARED_PTR
#define SHARED_PTR
#include <cstddef>

struct ControlBlock {
  size_t strong_count = 1; // 强引用计数
  size_t weak_count = 0;   // 弱引用计数
};
template <typename T> class SharedPtr;

template <typename T> class WeakPtr;

template <typename T> class SharedPtr {
  friend class WeakPtr<T>;

public:
  // 默认构造函数
  SharedPtr() : _p(nullptr), _ctrl(nullptr) {}

  // 带指针的构造函数
  explicit SharedPtr(T *p) : _p(p), _ctrl(new ControlBlock{}) {}

  SharedPtr(T *p, ControlBlock *ctrl) : _p(p), _ctrl(ctrl) {
    if (_ctrl) {
      ++_ctrl->strong_count; // 增加强引用计数
    } else {
      // 处理空指针的情况，比如抛出异常或做出警告
      throw std::runtime_error("SharedPtr control block is nullptr");
    }
  }

  // 拷贝构造函数
  SharedPtr(const SharedPtr &other) : _p(other._p), _ctrl(other._ctrl) {
    if (_ctrl) {
      ++_ctrl->strong_count;
    } else {
      // 处理空指针的情况，比如抛出异常或做出警告
      throw std::runtime_error("SharedPtr control block is nullptr");
    }
  }

  // 移动构造函数
  SharedPtr(SharedPtr &&other) noexcept : _p(other._p), _ctrl(other._ctrl) {
    other._p = nullptr;
    other._ctrl = nullptr;
  }

  // 析构函数
  ~SharedPtr() { reset(); }

  // 拷贝赋值运算符
  SharedPtr &operator=(const SharedPtr &other) {
    if (this != &other) {
      reset();
      _p = other._p;
      _ctrl = other._ctrl;
      if (_ctrl) {
        ++_ctrl->strong_count;
      }
    }
    return *this;
  }

  // 移动赋值运算符
  SharedPtr &operator=(SharedPtr &&other) noexcept {
    if (this != &other) {
      reset();
      _p = other._p;
      _ctrl = other._ctrl;
      other._p = nullptr;
      other._ctrl = nullptr;
    }
    return *this;
  }

  // 获取强引用计数
  size_t use_count() const { return (_ctrl) ? _ctrl->strong_count : 0; }

  void reset() {
    if (_ctrl) {
      --_ctrl->strong_count;
      if (_ctrl->strong_count == 0) {
        delete _p;
        if (_ctrl->weak_count == 0) {
          delete _ctrl;
        }
      }
    }
    _p = nullptr;
    _ctrl = nullptr;
  }

  void reset(T *p) {
    reset();
    if (p) {
      _p = p;
      if (!_ctrl) {
        _ctrl = new ControlBlock{};
      } else {
        _ctrl->strong_count = 1;
      }
    }
  }

  // 获取原始指针
  T *get() const { return _p; }

  T &operator*() const { return *_p; }

  T *operator->() const { return _p; }

  operator bool() const {
    return _p != nullptr; // 如果指针非空，则返回 true
  }

private:
  T *_p;
  ControlBlock *_ctrl;
};

template <typename T, typename... Args>
SharedPtr<T> make_shared(Args &&...args) {
  return SharedPtr<T>(new T(std::forward<Args>(args)...));
}
#endif // SHARED_PTR