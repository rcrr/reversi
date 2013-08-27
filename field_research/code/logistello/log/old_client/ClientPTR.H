#ifndef auto_ptr_H
#define auto_ptr_H

template <class X> class ClientPTR {
private:
  mutable X* ptr;
  mutable bool owns;
public:
  typedef X element_type;

  explicit ClientPTR(X* p = 0) : ptr(p), owns(p) {}

  ClientPTR(const ClientPTR& a) : ptr(a.ptr), owns(a.owns) {
    a.owns = 0;
  }

  ClientPTR& operator=(const ClientPTR& a) {
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

  template <class T> ClientPTR(T *p) {
    ptr = p;
    owns = (p);
  }

  template <class T> ClientPTR& operator=(T* a)  {
    if (a != ptr && owns) delete ptr;
    ptr = a;
    owns = (a);
    return *this;
  }

  template <class T> ClientPTR& operator=(ClientPTR<T>& a)  {
    if (&a != this) {
      if (owns) delete ptr;
      owns = a.owns;
      ptr = a.ptr;
      a.owns = 0;
      a.ptr = 0;    // don't allow simultaneous access
    }
    return *this;
  }
  
  ~ClientPTR() {
    if (owns) delete ptr;
  }

  X& operator*() const { return *ptr; }
  X* operator->() const { return ptr; }
  X* get() const { return ptr; }
  X* release() const { owns = false; return ptr; }
};

#endif
