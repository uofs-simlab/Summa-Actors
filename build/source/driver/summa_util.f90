! SUMMA - Structure for Unifying Multiple Modeling Alternatives
! Copyright (C) 2014-2020 NCAR/RAL; University of Saskatchewan; University of Washington
!
! This file is part of SUMMA
!
! For more information see: http://www.ral.ucar.edu/projects/summa
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module summa4chm_util
! utilities to manage summa simulation

! data types
USE nrtype                              ! high-level data types

! global data
USE globalData,only:integerMissing      ! missing integer value
USE globalData,only:realMissing         ! missing double precision value

! provide access to file IDs
USE globalData,only:ncid               ! file id of netcdf output file

! privacy
implicit none
private

! routines to make public
public::stop_program
public::handle_err
contains

 ! **************************************************************************************************
 ! error handler
 ! **************************************************************************************************
 ! TODO: will need to change how output files are closed
 subroutine handle_err(err,message)
 USE netcdf_util_module,only:nc_file_close             ! module to handle netcdf stuff for inputs and outputs
 implicit none
 ! dummy variables
 integer(i4b),intent(in)            :: err             ! error code
 character(*),intent(in)            :: message         ! error message
 ! local variables
 integer(i4b)                       :: iFreq           ! loop through output frequencies
 integer(i4b)                       :: nc_err          ! error code of nc_close
 character(len=256)                 :: cmessage        ! error message of the downwind routine
 ! ---------------------------------------------------------------------------------------
 ! return if A-OK
 if(err==0) return

 ! process error messages
 if (err>0) then
  write(*,'(//a/)') 'FATAL ERROR: '//trim(message)
 else
  write(*,'(//a/)') 'WARNING: '//trim(message); print*,'(can keep going, but stopping anyway)'
 endif

 ! close any remaining output files
 do iFreq = 1,size(ncid)
  if (ncid(iFreq)/=integerMissing) then
   call nc_file_close(ncid(iFreq),nc_err,cmessage)
   if(nc_err/=0) print*, trim(cmessage)
  end if
 end do

 stop 1
 end subroutine handle_err

 ! **************************************************************************************************
 ! stop_program: stop program execution
 ! **************************************************************************************************
 subroutine stop_program(err,message)
 ! used to stop program execution
 ! desired modules
 USE netcdf                                            ! netcdf libraries
 USE time_utils_module,only:elapsedSec                 ! calculate the elapsed time
 ! global data
 USE globalData,only: nThreads                         ! number of threads
 USE globalData,only: startInit                        ! date/time for the start of the initialization
 USE globalData,only: elapsedInit                      ! elapsed time for the initialization
 USE globalData,only: elapsedSetup                     ! elapsed time for the parameter setup
 USE globalData,only: elapsedRestart                   ! elapsed time to read the restart data
 USE globalData,only: elapsedRead                      ! elapsed time for the data read
 USE globalData,only: elapsedWrite                     ! elapsed time for the stats/write
 USE globalData,only: elapsedPhysics                   ! elapsed time for the physics
 implicit none
 ! define dummy variables
 integer(i4b),intent(in)            :: err             ! error code
 character(*),intent(in)            :: message         ! error messgage
 ! define the local variables
 integer(i4b),parameter             :: outunit=6       ! write to screen
 integer(i4b)                       :: endModelRun(8)  ! final time
 integer(i4b)                       :: localErr        ! local error code
 integer(i4b)                       :: iFreq           ! loop through output frequencies
 real(dp)                           :: elpSec          ! elapsed seconds

 ! close any remaining output files
 ! NOTE: use the direct NetCDF call with no error checking since the file may already be closed
 do iFreq = 1,size(ncid)
  if (ncid(iFreq)/=integerMissing) localErr = nf90_close(ncid(iFreq))
 end do

 ! get the final date and time
 call date_and_time(values=endModelRun)
 elpSec = elapsedSec(startInit,endModelRun)

 ! print initial and final date and time
 write(outunit,"(/,A,I4,'-',I2.2,'-',I2.2,2x,I2,':',I2.2,':',I2.2,'.',I3.3)") 'initial date/time = ',startInit(1:3),  startInit(5:8)
 write(outunit,"(A,I4,'-',I2.2,'-',I2.2,2x,I2,':',I2.2,':',I2.2,'.',I3.3)")   '  final date/time = ',endModelRun(1:3),endModelRun(5:8)

 ! print elapsed time for the initialization
 write(outunit,"(/,A,1PG15.7,A)")                                             '     elapsed init = ', elapsedInit,           ' s'
 write(outunit,"(A,1PG15.7)")                                                 '    fraction init = ', elapsedInit/elpSec

 ! print elapsed time for the parameter setup
 write(outunit,"(/,A,1PG15.7,A)")                                             '    elapsed setup = ', elapsedSetup,          ' s'
 write(outunit,"(A,1PG15.7)")                                                 '   fraction setup = ', elapsedSetup/elpSec

 ! print elapsed time to read the restart data
 write(outunit,"(/,A,1PG15.7,A)")                                             '  elapsed restart = ', elapsedRestart,        ' s'
 write(outunit,"(A,1PG15.7)")                                                 ' fraction restart = ', elapsedRestart/elpSec

 ! print elapsed time for the data read
 write(outunit,"(/,A,1PG15.7,A)")                                             '     elapsed read = ', elapsedRead,           ' s'
 write(outunit,"(A,1PG15.7)")                                                 '    fraction read = ', elapsedRead/elpSec

 ! print elapsed time for the data write
 write(outunit,"(/,A,1PG15.7,A)")                                             '    elapsed write = ', elapsedWrite,          ' s'
 write(outunit,"(A,1PG15.7)")                                                 '   fraction write = ', elapsedWrite/elpSec

 ! print elapsed time for the physics
 write(outunit,"(/,A,1PG15.7,A)")                                             '  elapsed physics = ', elapsedPhysics,        ' s'
 write(outunit,"(A,1PG15.7)")                                                 ' fraction physics = ', elapsedPhysics/elpSec

 ! print total elapsed time
 write(outunit,"(/,A,1PG15.7,A)")                                             '     elapsed time = ', elpSec,                ' s'
 write(outunit,"(A,1PG15.7,A)")                                               '       or           ', elpSec/60_dp,          ' m'
 write(outunit,"(A,1PG15.7,A)")                                               '       or           ', elpSec/3600_dp,        ' h'
 write(outunit,"(A,1PG15.7,A/)")                                              '       or           ', elpSec/86400_dp,       ' d'

 ! print the number of threads
 write(outunit,"(A,i10,/)")                                                   '   number threads = ', nThreads

 ! stop with message
 if(err==0)then
  print*,'FORTRAN STOP: '//trim(message)
  stop
 else
  print*,'FATAL ERROR: '//trim(message)
  stop 1
 endif

 end subroutine

end module summa4chm_util
