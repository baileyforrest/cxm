#pragma once

#include <cassert>
#include <cstdint>
#include <utility>

namespace internal {

class RcBase {
 protected:
  struct Rep {
    uint32_t ref_cnt;
    char val[];
  };

  template <typename T>
  static size_t GetPad() {
    if (alignof(T) <= sizeof(Rep)) {
      return 0;
    }

    return alignof(T) - sizeof(Rep);
  }

  RcBase() = default;
  explicit RcBase(Rep* rep) : rep_(rep) {}

  RcBase(const RcBase& other) { *this = other; }
  RcBase(RcBase&& other) { *this = other; }

  RcBase& operator=(const RcBase& other) {
    DecRef();
    rep_ = other.rep_;
    if (rep_ != nullptr) {
      ++rep_->ref_cnt;
    }
    return *this;
  }
  RcBase& operator=(RcBase&& other) {
    rep_ = other.rep_;
    return *this;
  }

  virtual ~RcBase() = default;

  virtual void Destroy() = 0;

  void DecRef() {
    if (rep_ == nullptr) {
      return;
    }

    assert(rep_->ref_cnt > 0);
    if (--rep_->ref_cnt == 0) {
      Destroy();
      ::operator delete(rep_);
    }
  }

 private:
  Rep* rep_ = nullptr;
};

}  // namespace internal

// Simple reference counting smart pointer. We don't use std::shared_ptr because
// we don't want/need atomic semantics.
//
// This combined with inlining lets us not worry about move semantics to reduce
// some boilerplate.
template <typename T>
class Rc final : public internal::RcBase {
 public:
  Rc() = default;
  ~Rc() { DecRef(); }

  template <typename... Args>
  static Rc Make(Args&&... args) {
    size_t size = sizeof(Rep) + GetPad<T>() + sizeof(T);
    auto* rep = reinterpret_cast<Rep*>(::operator new(size));
    rep->ref_cnt = 1;

    auto* val = reinterpret_cast<T*>(rep->val + GetPad<T>());
    new (val) T(std::forward<Args>(args)...);

    return Rc(rep, val);
  }

  template <typename T2>
  Rc(const Rc<T2>& other) : RcBase(static_cast<const RcBase&>(other)) {
    static_assert(std::is_base_of_v<T, T2>);
    val_ = static_cast<T*>(other.get());
  }
  template <typename T2>
  Rc(Rc<T2>&& other) : RcBase(static_cast<RcBase&&>(other)) {
    static_assert(std::is_base_of_v<T, T2>);
    val_ = static_cast<T*>(other.get());
  }

  template <typename T2>
  Rc& operator=(const Rc<T>& other) {
    static_assert(std::is_base_of_v<T, T2>);
    *static_cast<RcBase*>(this) = static_cast<const RcBase&>(other);
    val_ = static_cast<T*>(other.get());
  }
  template <typename T2>
  Rc& operator=(Rc<T>&& other) {
    static_assert(std::is_base_of_v<T, T2>);
    *static_cast<RcBase*>(this) = static_cast<RcBase&&>(other);
    val_ = static_cast<T*>(other.get());
  }

  explicit operator bool() const { return val_ != nullptr; }

  T* get() { return val_; }
  T& operator*() { return *get(); }
  T* operator->() { return get(); }

  T* get() const { return val_; }
  const T& operator*() const { return *get(); }
  const T* operator->() const { return get(); }

 private:
  explicit Rc(Rep* rep, T* val) : RcBase(rep), val_(val) {}

  void Destroy() override { val_->~T(); }

  T* val_ = nullptr;
};
