// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef auto_ptr_H
#define auto_ptr_H

template <class X> class PTR {
private:
  mutable X* ptr;
  mutable bool owns;
public:
  typedef X element_type;

  explicit PTR(X* p = 0) : ptr(p), owns(p) {}

  PTR(const PTR& a) : ptr(a.ptr), owns(a.owns) {
    a.owns = 0;
  }

  PTR& operator=(const PTR& a) {
    if (&a != this) {
      if (owns)
        delete ptr;
      owns = a.owns;
      ptr = a.ptr;
      a.owns = 0;
      a.ptr = 0;    // don't allow simultaneous access
    }
    return *this;
  }

  template <class T> PTR(T *p) {
    ptr = p;
    owns = (p);
  }

  template <class T> PTR& operator=(T* a)  {
    if (a != ptr && owns) delete ptr;
    ptr = a;
    owns = (a);
    return *this;
  }

  template <class T> PTR& operator=(PTR<T>& a)  {
    if (&a != this) {
      if (owns) delete ptr;
      owns = a.owns;
      ptr = a.ptr;
      a.owns = 0;
      a.ptr = 0;    // don't allow simultaneous access
    }
    return *this;
  }
  
  ~PTR() {
    if (owns) delete ptr;
  }

  X& operator*() const { return *ptr; }
  X* operator->() const { return ptr; }
  X* get() const { return ptr; }
  X* release() const { owns = false; return ptr; }
};

#endif
