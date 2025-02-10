module ini
!
! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Initialisation module for Fortran expedition simulator.            |
! |                                                                    |
! | license : MIT                                                      |
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.eu)           |
! |--------------------------------------------------------------------|
!
! load modules
  use :: typ

! basic options
  implicit none
  private

! declare public
  public :: INI_io

! ==== Declarations

! game mechanics
  type(TYP_io) :: INI_io

! ==== Data
! game mechanics
  data INI_io%pUnit/6/, INI_io%wUnit/21/

end module ini
