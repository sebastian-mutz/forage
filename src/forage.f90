module forage
  use :: typ
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, forage!"
  end subroutine say_hello
end module forage
