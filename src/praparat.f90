module praparat_module
    !$ use omp_lib
    use iso_c_binding
    use math
    Implicit none

    type :: Character_len1
        Character(len=1) :: c
    end type Character_len1

    Integer :: RANDOM_SEED_FLAG = 0
    Integer :: FIELD_INIT_FLAG = 1
    Integer :: FIELD_ADJUST_FLAG = 0
    Integer :: SUN_INIT_FLAG = 1
    Integer :: NIGHT_FLAG = 0
    Integer :: TRACE_FLAG = 0
    Integer :: FIX_FLAG = 0
    Integer :: FRAGMENT_FLAG = 0

    Integer, allocatable :: TRACE_LIST(:)
    Integer :: TRACE_NUM

    Integer, allocatable :: FIX_LIST(:)
    Double precision, allocatable :: FIX_ENERGY(:)
    Integer :: FIX_NUM

    Integer, allocatable :: FRAGMENT_LIST(:, :)
    Integer :: FRAGMENT_NUM
    Integer :: FRAGMENT_INTERVAL_STEP = 1
    Integer, parameter :: FRAGMENT_MAX_LENGTH = 32
    
    !Character(len=1),  parameter :: P = '+'
    !Character(len=1),  parameter :: M = '-'
    Character(len=64), parameter :: CODES = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-'

    Double precision :: WORLD_DT = 0.01d0
    Double precision :: WORLD_G = 10.0d0
    Double precision :: BLOCK_EMD_FORCE_K = 2000.0d0
    Double precision :: BLOCK_EMD_FORCE_R = 10.0d0
    Double precision :: CELL_EMD_FORCE_K = 200.0d0
    Double precision :: CELL_EMD_FORCE_R = 4.0d0
    Double precision :: AIR_RESISTANCE_K = 0.01d0

    Integer, parameter :: BOOK_MAX_LENGTH = 65536
    Integer, parameter :: BOOKMARKER_LENGTH = 2
    Integer, parameter :: BOOKMARKER_ADVANCE_LENGTH = 1

    Character(len=16), parameter :: EXPANSION_FLAG       = 'ABCDEFGHIJKLMNOP'
    Character(len=16), parameter :: CONNECT_FLAG         = 'QRSTUVWXYZabcdef'
    Character(len=16), parameter :: DISCONNECT_FLAG      = 'ghijklmnopqrstuv'
    Character(len=16), parameter :: TURN_BOOKMARKER_FLAG = 'wxyz0123456789+-'

    Character(len=3), parameter :: BD_BOOKMARKER_ADVANCE_S = 'np2'!Book Decode bookmarker advance Setting
    
    Character(len=3), parameter :: BDWS = 'bp1'!Book Decode W Setting
    Integer, parameter :: BDWD = 2!Book Decode W Digits
    Double precision, parameter :: BDWC = 1.0d0!Book Decode W Coefficient

    Character(len=3), parameter :: BDAS = 'np0'!Book Decode ACOLOR Setting
    Integer, parameter :: BDAD = 2!Book Decode ACOLOR Digits
    Double precision, parameter :: BDAC = 1.0d0!Book Decode ACOLOR Coefficient

    Character(len=3), parameter :: BDIS = 'np2'!Book Decode I Setting
    Integer, parameter :: BDID = 2!Book Decode I Digits
    Double precision, parameter :: BDIC = 1.0d0!Book Decode I Coefficient

    !np2:0.00, 0.01~40.3
    !np1:0.000, 0.001~4.03
    Character(len=3), parameter :: BDMS = 'np1'!Book Decode M Setting 
    Integer, parameter :: BDMD = 2!Book Decode M Digits
    Double precision, parameter :: BDMC = 1.0d0!Book Decode M Coefficient
    Double precision, parameter :: MIN_M = 0.5d0!Minimum M
    Double precision, parameter :: MAX_M = 4.03d0!Minimum M

    Character(len=3), parameter :: BDRS = 'np0'!Book Decode R Setting
    Integer, parameter :: BDRD = 2!Book Decode R Digits
    Double precision :: BDRC = 1.0d0!Book Decode R Coefficient
    Double precision :: MIN_R = 0.2d0!Minimum R

    Double precision, parameter :: MIN_I = 0.1d0!Minimum M

    Integer, parameter :: NOCC = 8  !Number Of Connected Cell

    Integer, parameter :: NOSO = 4!Number Of Sense Organ
    Integer, parameter :: IDUIO_LR = 3*NOCC + 1     !In Data Unit ID Of Light R
    Integer, parameter :: IDUIO_LG = 3*NOCC + 2     !In Data Unit ID Of Light G
    Integer, parameter :: IDUIO_LB = 3*NOCC + 3     !In Data Unit ID Of Light B
    Integer, parameter :: IDUIO_TOUCH = 3*NOCC + 4  !In Data Unit ID Of TOUCH

    Integer, parameter :: NOA = 4!Number Of Action
    Integer, parameter :: ODUIO_TRANSF = 3*NOCC + 1 !Out Data Unit ID Of Transformation
    Integer, parameter :: ODUIO_EAT = 3*NOCC + 2    !Out Data Unit ID Of Eat
    Integer, parameter :: ODUIO_FUSION = 3*NOCC + 3 !Out Data Unit ID Of Fusion
    Integer, parameter :: ODUIO_LIGHT = 3*NOCC + 4 !Out Data Unit ID Of Light

    Integer, parameter :: NOIU = 3*NOCC + NOSO  !Number Of Input Units
    Integer, parameter :: NOHU = 4 !Number Of Hidden Units
    Integer, parameter :: NOOU = 3*NOCC + NOA !Number Of Output Units

    !Need Book Length To Expansion
    !3 + 3 + 3 + 1 + 3 + 1 = 14
    !4*(2*BOOKMARKER_LENGTH - 1) + 2*BOOKMARKER_ADVANCE_LENGTH = 14
    Integer, parameter :: NBLTE = 3*BDAD + 3*BDID + BDMD + BDRD + BDWD*(NOHU*(NOIU+1) + NOOU*(NOHU+1)) + 14
    !Need Book Length To Connect
    Integer, parameter :: NBLTC = 2*BOOKMARKER_LENGTH-1 + 2*BOOKMARKER_ADVANCE_LENGTH - 1
    !Need Book Length To Disonect
    Integer, parameter :: NBLTD = 2*BOOKMARKER_LENGTH-1 + 2*BOOKMARKER_ADVANCE_LENGTH - 1
    !Need Book Length To Turn Book Marker
    Integer, parameter :: NBLTT = 2*BOOKMARKER_LENGTH-1 + 2*BOOKMARKER_ADVANCE_LENGTH - 1

    Logical :: EVERY_STEP_COST_UPDATE_FLAG = .FALSE.
    Integer :: TARGET_CELLS_NUM
    Double precision, parameter :: LIMIT_E = 0.01d0
    Double precision :: EVERY_STEP_COST_E = 0.01d0
    Double precision :: EVERY_STEP_COST_A = 2.0d0
    Double precision :: EVERY_STEP_COST_D = 1.0d-7 ! 1.0d-5
    Integer :: EVERY_STEP_COST_M = 0
    Double precision :: ALONE_COST_E_COEFF = 0.1d0
    Double precision :: ORG_EVERY_STEP_COST_E = 0.01d0
    Double precision, allocatable :: EVERY_STEP_COST_LIST(:)

    Double precision, parameter :: INIT_K = 1000.0d0
    Double precision, parameter :: INIT_L_C = 1.0d0
    Double precision, parameter :: INIT_S = 1.0d0

    Double precision :: ENERGY_TRANSIT_MIN = 1.0d0
    Double precision :: ENERGY_TRANSIT_C = 0.1d0
    Double precision :: SPRING_LIMIT_C = 2.0d0 ! SPRING_LIMIT_C*r
    Double precision :: SPRING_CATCH_C = 2.0d0 ! SPRING_CATCH_C*(r1+r2)
    Double precision :: SPRING_BREAK_C = 2.0d0 ! SPRING_BREAK_C*(r1+r2)

    Double precision :: RATE_OF_VARI_L = 0.2d0
    Double precision :: RANGE_OF_VARI_L_D = 0.7d0
    Double precision, parameter :: LIMIT_S = 1.0d0
    Double precision :: RATE_OF_VARI_S = 0.1d0
    Double precision, parameter :: CELL_RECEIVE_TOUCH = 0.5d0

    Double precision :: INFO_TRANS_EMI_C = 100.0d0 !Information Transmitter Emission C
    Double precision :: INFO_TRANS_R = 0.2d0
    Double precision, parameter :: INFO_TRANS_M = 1.0d0
    Double precision, parameter :: INFO_TRANS_SPEED = 20.0d0
    Double precision, parameter :: INFO_TRANS_I_MIN = 0.01d0
    Double precision, parameter :: INFO_TRANS_BLOCK_EMD_FORCE_K = 10.0d0
    Double precision, parameter :: INFO_TRANS_BLOCK_EMD_FORCE_R = 0.01d0
    Double precision :: CONTACT_AREA_CORR = 0.0d0

    !Energy Conversion Efficiency Of Light
    Double precision :: ECEOL_E2L = 2000.0d0
    Double precision :: ECEOL_L2E = 500.0d0
    Double precision :: ECEOLR = 0.8d0
    Double precision :: ECEOLG = 0.1d0
    Double precision :: ECEOLB = 0.8d0

    !Energy Conversion Efficiency Of Eat
    Double precision :: ECEOE = 0.1d0

    !Energy Conversion Efficiency Of Fusion
    Double precision, parameter :: ECEOF = 1.0d0

    Double precision :: GENE_COST_C = 0.1d0
    Double precision :: CONN_COST_C = 0.0d0

    Double precision :: THRESHOLD_EAT    = 0.0d0
    Double precision :: THRESHOLD_FUSION = 0.0d0
    Double precision :: THRESHOLD_LIGHT  = 0.0d0

    Integer :: MUTATION_RATE_FLAG = 0

    Integer :: CELLS_INIT_FLAG = 0
    Integer :: NUMBER_OF_CELL = 50000
    Integer :: ABS_KEEP_NUM = 100
    Integer :: SUN_ID = 1
    Integer :: SUM_LIMIT_NUM = 20000
    Integer :: ENERGY_TRANSIT_FLAG = 2
    Double precision :: SUN_ENERGY = 1000000000000.0d0
    Double precision :: SUN_CYCLE = 500.0d0
    Double precision :: SUN_AMP   = 0.4d0
    Double precision :: MUTATION_DIVISION_RATE = 0.1d0
    Double precision :: MUTATION_RATE_AMP = 0.5d0
    Double precision :: MUTATION_RATE_THR = 0.4d0
    Double precision :: MUTATION_RATE_T = 10000.0d0
    Double precision :: MUTATION_CENTER = 0.05d0
    Double precision :: MUTATION_COEFF4EXPANSION = 2.0d0

    Integer, parameter :: WALL_VALUE = 1
    Integer, parameter :: IVWALL_VALUE = 2
    Double precision, parameter :: UNIT_WALL_LENGTH = 1.0d0
    Double precision, parameter :: UNIT_WALL_R = ((sqrt(2.0d0) + 1.0d0)/2.0d0)*(1.0d0/2.0d0)
    Double precision, parameter :: WALL_ar = 0.8d0
    Double precision, parameter :: WALL_ag = 0.8d0
    Double precision, parameter :: WALL_ab = 0.8d0

    Integer, parameter :: CELL_CAPACITY_OF_BLOCK = 128

    Integer :: FIELD_SIZE_X = 128
    Integer :: FIELD_SIZE_Y = 128
    Integer :: FIELD_SIZE_Z = 128

    Integer :: FIELD_CENTER_X = 0
    Integer :: FIELD_CENTER_Y = 16
    Integer :: FIELD_CENTER_Z = 0

    Integer :: DEPTH_LIMIT = 2
    Double precision :: HEIGHT_LIMIT = -1.0d0

    Double precision :: FIELD_TOP = 0.0d0
    Double precision :: CELLS_BOTTOM = 0.0d0
    Double precision :: DISTANCE2SUN = 40.0d0

    Logical :: LOG_FLAG = .FALSE.

    type :: map
        Integer :: wall
        Integer :: NOC
        Integer :: IOC(CELL_CAPACITY_OF_BLOCK)
        Double precision :: x, y, z
        Double precision :: cr, cg, cb
        Double precision :: ar, ag, ab
    end type map

    type :: cell
        Integer :: exist

        Character(len=BOOK_MAX_LENGTH) :: book
        Character(len=BOOKMARKER_LENGTH) :: bookmarker
        Character(len=BOOKMARKER_ADVANCE_LENGTH) :: bookmarker_advance

        Double precision :: x, y, z
        Integer :: nx, ny, nz
        Double precision :: vx, vy, vz
        Double precision :: fx, fy, fz
        Double precision :: cr, cg, cb
        Double precision :: ar, ag, ab
        Double precision :: IOLr  !Intensity Of Light r
        Double precision :: IOLg  !Intensity Of Light g
        Double precision :: IOLb  !Intensity Of Light b
        Double precision :: m
        Double precision :: r
        Double precision :: E
        Double precision :: alpha

        Integer :: ID
        Integer(8) :: age

        Double precision :: in_data(NOIU)
        Double precision :: out_data(NOOU)
        Double precision :: w1(NOHU,(NOIU+1))
        Double precision :: w2(NOOU,(NOHU+1))

        Double precision :: KOCC(NOCC)    !K Of Connected Cell
        Double precision :: LOCC(NOCC)    !L Of Connected Cell
        Double precision :: SOCC(NOCC)    !S Of Connected Cell

        Integer :: CCF(NOCC)      !Connected Cell Flag
        Integer :: IOCC(NOCC)     !Id Of Connected Cell
        Integer :: UIOCC(NOCC)    !Unit Id Of Connected Cell

        Integer :: HIT_BLOCK_AF
        Integer :: HIT_CELL_AF
        Integer :: SPRING_AF
        Integer :: MECHANICS_AF

        Integer :: EAT_AF
        Integer :: FUSION_AF
        Integer :: LIGHT_AF

        Integer :: INFO_TRANS_F
        Integer :: NEURAL_NETWORK_F

        Integer :: WAIT_FOR_CONNECT_F
        Integer :: WAIT_FOR_CONNECT_UI
        Integer :: WAIT_FOR_DISCONNECT_F

        Integer :: ALONE_F
        
    end type cell

    Integer :: WORLD_STEP = 1
    Integer :: TRANS_INTERVAL_STEP = 100
    Integer :: RESET_INTERVAL_STEP = 5
    Integer :: OUT_INTERVAL_STEP = 10

    Integer, allocatable :: CONN_CL(:,:,:)
    Integer :: CONN_CLL
    Integer :: CONN_CLL_MAX

    Integer, allocatable :: BACK_CONN_CL(:,:,:)
    Integer :: BACK_CONN_CLL

    Integer, allocatable :: CALC_CL(:)! Calculation Cell List
    Integer :: CALC_CLL! Calculation Cell List Length

    Integer, allocatable :: BACK_CALC_CL(:)! Calculation Cell List
    Integer :: BACK_CALC_CLL! Calculation Cell List Length

    Integer, allocatable :: NOT_CALC_CL(:)
    Integer :: NOT_CALC_CLL
    Integer :: NEW_NOT_CALC_CLL

    type(map), allocatable :: FIELD(:, :, :)

    type(map), allocatable :: block_list(:)
    Integer :: block_list_length

    Integer :: NUMBER_OF_INFO_TRANS

    type(cell), allocatable :: CELLS(:)

    Integer :: number_of_threads = 4

    Integer :: EAT_COUNT = 0
    Integer :: FUSION_COUNT = 0
    Integer :: EXPANSION_COUNT = 0
    Integer :: CONNECT_COUNT = 0
    Integer :: DISCONNECT_COUNT = 0
    Integer :: TURN_BOOKMARKER_COUNT = 0
    Integer :: MUTATION_COUNT = 0
    Integer :: DEATHS_COUNT = 0
    Integer :: CHANGE_L_COUNT = 0
    Integer :: ENERGY_TRANSIT_COUNT = 0
    Integer :: ALONE_COUNT = 0
    Integer :: NUMBER_OF_CELLS_WOIT = 0

    Double precision :: TOT_ENERGY      = 0.0d0
    Double precision :: TOT_ENERGY_FOR_C = 0.0d0
    Double precision :: AGE_AVE    = 0.0d0
    Double precision :: AGE_VAR    = 0.0d0
    Integer(8) :: AGE_MIN    = 0
    Integer(8) :: AGE_MAX    = 0
    Double precision :: AGE_IT_AVE    = 0.0d0
    Double precision :: AGE_IT_VAR    = 0.0d0
    Integer(8) :: AGE_IT_MIN    = 0
    Integer(8) :: AGE_IT_MAX    = 0
    Double precision :: CENTER_OF_GRAV_X = 0.0d0
    Double precision :: CENTER_OF_GRAV_Y = 0.0d0
    Double precision :: CENTER_OF_GRAV_Z = 0.0d0
    Double precision :: TOT_M            = 1.0d0
    Integer :: ENERGY_N = 0
    Integer :: EAT_INFO(NOCC+1)
    Integer :: EATEN_INFO(NOCC+1)
    Integer, allocatable :: AGE_COUNT(:)

contains

    function decode(code, option) result(f)
        Character(len=*) :: code
        Character(len=*) :: option
        Double precision :: f, max
        Integer :: i, j, N

        f = 0.0d0
        N = len_trim(code)
        max = 64.0d0**N

        Do i=1, N
            Do j=1, 64
                If(CODES(j:j)==code(i:i)) then
                    exit
                end If
            end Do
            f = f + dble(j-1)*(64.0d0**(N-i))
        end Do

        Do i=1, 100
            If((max/(10.0d0**(i-1)))<1.0d0) then
                exit
            end If
        end Do

        If(option(2:2) == 'p') then
            read (option(3:),*) j
            If(option(1:1) == 'b') then
                f = (f - max/2.0d0)/(10.0d0**(i-1-j))
            else If(option(1:1) == 'n') then
                f = f/(10.0d0**(i-1-j))
            end If
        else If(option(2:2) == 'm') then
            read (option(3:),*) j
            If(option(1:1) == 'b') then
                f = (f - max/2.0d0)/(10.0d0**(i-1+j))
            else If(option(1:1) == 'n') then
                f = f/(10.0d0**(i-1+j))
            end If
        end If

    end function decode

    function quote(string, n) result(s)
        Integer :: i, n, l
        Character(len=n) :: s
        type(Character_len1) :: string(:)

        If(size(string(:), 1)<n) then
            l = size(string(:), 1)
        else
            l = n
        end If

        Do i=1, l
            s(i:i) = string(i)%c
        end Do

    end function quote

    function random_code(n) result(s)
        Integer :: i, c, n
        Character(len=n) :: s
        Double precision :: x
        Do i=1, n
            call random_number(x)
            c = nint(63.0d0*x) + 1
            s(i:i) = CODES(c:c)
        end Do
    end function random_code

    function calc_energy_inf(r, ar, ag, ab, cn) result(E_inf)
      Double precision :: r, ar, ag, ab
      Integer :: cn
      Double precision :: E_inf

      Double precision :: A, S, D
      Double precision :: dEr, dEg, dEb, dE
      
      A = (INFO_TRANS_EMI_C*(CELLS(SUN_ID)%r**2))/(4.0d0*PI*(DISTANCE2SUN**2))
      !S = PI*((r + 2.0d0*INFO_TRANS_R)**2)
      S = PI*((r + CONTACT_AREA_CORR + INFO_TRANS_R)**2)
      
      dEr = ECEOLR*CELLS(SUN_ID)%IOLr*ar
      dEg = ECEOLG*CELLS(SUN_ID)%IOLg*ag
      dEb = ECEOLB*CELLS(SUN_ID)%IOLb*ab
      dE  = ECEOL_L2E*(dEr + dEg + dEb)

      !D = ORG_EVERY_STEP_COST_E
      D = ORG_EVERY_STEP_COST_E*(EVERY_STEP_COST_A**(cn-NOCC)) 
      
      E_inf = dE/(1.0d0 - exp(-D/(A*S)))
    end function calc_energy_inf

    subroutine set_every_step_cost_list()
      Integer :: cn

      do cn=0, NOCC
         EVERY_STEP_COST_LIST(cn+1) = EVERY_STEP_COST_E*(EVERY_STEP_COST_A**(abs(cn-EVERY_STEP_COST_M)-8))
      end do

    end subroutine set_every_step_cost_list
    
    subroutine return_mutation_rate(x, z, mr) 
      Double precision :: x, z
      Double precision :: mr
      Double precision :: r, rr, fr, a
      mr = 0.0d0
      if(MUTATION_RATE_FLAG==0) then
         !Setting 0
         mr = MUTATION_RATE_AMP
      else if(MUTATION_RATE_FLAG==1) then
         !Setting 1
         fr = sqrt(dble(FIELD_SIZE_X**2 + FIELD_SIZE_Z**2))
         rr = x**2 + z**2
         r = sqrt(rr)
         If(r<fr*MUTATION_CENTER) then
            mr = 0.0d0
         else 
            a = MUTATION_RATE_AMP/(fr*(1.0d0-MUTATION_CENTER))
            mr = a*(r - fr*MUTATION_CENTER)
         end If
      else if(MUTATION_RATE_FLAG==2) then
         !Setting 2
         fr = dble(FIELD_SIZE_Z/2)
         If(z<fr*MUTATION_CENTER) then
            mr = 0.0d0
         else 
            a = MUTATION_RATE_AMP/(fr*(1.0-MUTATION_CENTER))
            mr = a*(z - fr*MUTATION_CENTER)
         end If
      else if(MUTATION_RATE_FLAG==3) then
         !Setting 3
         mr = MUTATION_RATE_AMP*sin(2.0d0*PI*(dble(WORLD_STEP)/MUTATION_RATE_T))
         if(mr<MUTATION_RATE_THR) then
            mr = 0.0d0
         end if
      end if
      
    end subroutine return_mutation_rate

    !https://qiita.com/aisha/items/c41c09b0587ba6503733
    function endswith(longtext, tailtext) result(ends)
        logical ends
        character(len=*), intent(in) :: longtext, tailtext
        character(len=256) longtext_, tailtext_
        integer long_eloc, tail_eloc
    
        ends = .FALSE.
        longtext_ = trim(longtext)
        tailtext_ = trim(tailtext)
        long_eloc = len_trim(longtext)
        tail_eloc = len_trim(tailtext)
        if (long_eloc == 0 .or. tail_eloc == 0) return
        if (long_eloc < tail_eloc) return
        if (longtext_(long_eloc - tail_eloc + 1:) == tailtext_) then
            ends = .TRUE.
        endif
    end function endswith

    !https://qiita.com/aisha/items/c41c09b0587ba6503733
    function dir_name(path) result(dir)
        character(len=256) dir
        character(len=*), intent(in) :: path
        integer iloc
        logical is_found
    
        if (endswith(path, '/')) then
            dir = path
            return
        endif
    
        is_found = .FALSE.
        do iloc = len_trim(path), 1, -1
            if (path(iloc:iloc) == '/') then
                is_found = .TRUE.
                exit
            endif
        enddo
    
        if (is_found) then
            dir = path(:(iloc-1))
        else
            dir = path
        endif
    end function dir_name

    !https://qiita.com/aisha/items/c41c09b0587ba6503733
    subroutine makedirs(outdir)
        character(len=*), intent(in) :: outdir
        character(len=256) command
        write(command, *) 'if [ ! -d ', trim(outdir), ' ]; then mkdir -p ', trim(outdir), '; fi'
        write(*, *) trim(command)
        call system(command)
    end subroutine makedirs

    subroutine removedirs(outdir)
        character(len=*), intent(in) :: outdir
        character(len=256) command
        write(command, *) 'if [ -d ', trim(outdir), ' ]; then rm -r ', trim(outdir), '; fi'
        write(*, *) trim(command)
        call system(command)
    end subroutine removedirs

    subroutine tardirs(outdir)
        character(len=*), intent(in) :: outdir
        character(len=256) command
        write(command, *) 'if [ -d ', trim(outdir), ' ]; then tar -zcvf ', &
             trim(adjustl(outdir))//'.tar.gz ', trim(outdir), '; fi'
        write(*, *) trim(command)
        call system(command)
    end subroutine tardirs

    subroutine removefile(outfile)
        character(len=*), intent(in) :: outfile
        character(len=256) command
        write(command, *) 'if [ -f ', trim(outfile), ' ]; then rm ', trim(outfile), '; fi'
        write(*, *) trim(command)
        call system(command)
    end subroutine removefile

    subroutine tarfile(outfile)
        character(len=*), intent(in) :: outfile
        character(len=256) command
        write(command, *) 'if [ -f ', trim(outfile), ' ]; then tar -zcvf ', &
             trim(adjustl(outfile))//'.tar.gz ', trim(outfile), '; fi'
        write(*, *) trim(command)
        call system(command)
    end subroutine tarfile

    subroutine sun_init(id)
        Integer :: id

        SUN_ID = id

        CELLS(id)%exist = 1
        CELLS(id)%x = 0.0d0
        CELLS(id)%y = DISTANCE2SUN
        CELLS(id)%z = 0.0d0

        CELLS(id)%nx = 0
        CELLS(id)%ny = 0
        CELLS(id)%nz = 0

        CELLS(id)%vx = 0.0d0
        CELLS(id)%vy = 0.0d0
        CELLS(id)%vz = 0.0d0

        CELLS(id)%fx = 0.0d0
        CELLS(id)%fy = 0.0d0
        CELLS(id)%fz = 0.0d0

        CELLS(id)%cr = 1.0d0
        CELLS(id)%cg = 1.0d0
        CELLS(id)%cb = 0.8d0

        CELLS(id)%ar = 1.0d0 - CELLS(id)%cr
        CELLS(id)%ag = 1.0d0 - CELLS(id)%cg
        CELLS(id)%ab = 1.0d0 - CELLS(id)%cb

        CELLS(id)%IOLr = 10.0d0
        CELLS(id)%IOLg = 10.0d0
        CELLS(id)%IOLb = 8.0d0

        CELLS(id)%m = INFO_TRANS_M
        CELLS(id)%r = 0.4d0
        CELLS(id)%E = SUN_ENERGY

        CELLS(id)%alpha = 0.0d0

        CELLS(id)%ID  = id
        CELLS(id)%age = 0

        CELLS(id)%in_data(:) = 0.0d0

        CELLS(id)%out_data(:) = 1.0d0

        CELLS(id)%w1(:,:) = 0.0d0

        CELLS(id)%w2(:,:) = 0.0d0

        CELLS(id)%KOCC(:) = 0.0d0

        CELLS(id)%LOCC(:) = 0.0d0

        CELLS(id)%SOCC(:) = 0.0d0

        CELLS(id)%IOCC(:) = 0

        CELLS(id)%UIOCC(:) = 0

        CELLS(id)%CCF(:) = 0

        CELLS(id)%HIT_BLOCK_AF = 0
        CELLS(id)%HIT_CELL_AF = 0
        CELLS(id)%SPRING_AF = 0
        CELLS(id)%MECHANICS_AF = 0
        CELLS(id)%EAT_AF = 0
        CELLS(id)%FUSION_AF = 0
        CELLS(id)%LIGHT_AF = 1
        CELLS(id)%INFO_TRANS_F = 0
        CELLS(id)%NEURAL_NETWORK_F = 0
        CELLS(id)%WAIT_FOR_CONNECT_F = 0
        CELLS(id)%WAIT_FOR_CONNECT_UI = 0
        CELLS(id)%WAIT_FOR_DISCONNECT_F = 0
        CELLS(id)%ALONE_F = 1

    end subroutine sun_init

    subroutine field_init()  bind(c)
      Integer :: i, j, k
      Integer :: write_flag = 0

      if(FIELD_INIT_FLAG/=0) then
         allocate(FIELD(FIELD_SIZE_X, FIELD_SIZE_Y, FIELD_SIZE_Z))
      
         FIELD(:, :, :)%wall = 0
         FIELD(:, :, :)%NOC = 0
         FIELD(:, :, :)%cr = 0.0d0
         FIELD(:, :, :)%cg = 0.0d0
         FIELD(:, :, :)%cb = 0.0d0
         FIELD(:, :, :)%ar = 0.5d0
         FIELD(:, :, :)%ag = 0.5d0
         FIELD(:, :, :)%ab = 0.5d0

         write_flag = 1
      end if

      if(FIELD_INIT_FLAG==0) then
         write(*, *) 'Read field from ./field.d'
         call read_field('./field.d')
      else if(FIELD_INIT_FLAG==1) then
         write(*, *) 'Create field as flat'
         call create_flat_field()
      else if(FIELD_INIT_FLAG==2) then
         write(*, *) 'Create field as random'
         call create_random_field(4)
      else if(FIELD_INIT_FLAG==3) then
         write(*, *) 'Create field as maze'
         call create_maze_field_w()
      else
         write(*, *) "FIELD_INIT_FLAG should be a number from 0 to 3."
         write(*, *) "0 : Read"
         write(*, *) "1 : Flat"
         write(*, *) "2 : Random"
         write(*, *) "3 : Maze"
         stop
      end if
      
      FIELD(1, :, :)%wall = 2
      FIELD(FIELD_SIZE_X, :, :)%wall = 2
      FIELD(:, 1, :)%wall = 2
      FIELD(:, FIELD_SIZE_Y, :)%wall = 2
      FIELD(:, :, 1)%wall = 2
      FIELD(:, :, FIELD_SIZE_Z)%wall = 2
      
      FIELD_TOP = 0.0d0
      Do i=1, FIELD_SIZE_X
         Do j=1, FIELD_SIZE_Y
            Do k=1, FIELD_SIZE_Z
               FIELD(i,j,k)%x = dble(i - FIELD_SIZE_X/2 + FIELD_CENTER_X) - 1.0d0
               FIELD(i,j,k)%y = dble(j - FIELD_SIZE_Y/2 + FIELD_CENTER_Y) - 1.0d0
               FIELD(i,j,k)%z = dble(k - FIELD_SIZE_Z/2 + FIELD_CENTER_Z) - 1.0d0
               if(FIELD(i, j, k)%wall==1 .and. FIELD_TOP<FIELD(i,j,k)%y) then
                  FIELD_TOP = FIELD(i,j,k)%y
               end if
            end Do
         end Do
      end Do
      
      If(write_flag==1) then
         write(*, *) 'Write field into ./field.d'
         call write_field('./field.d')
      end If
        
      call make_field_list()

    end subroutine field_init

    subroutine create_random_field(n)
        Integer :: i, j, n, mx, my, rpx, rpy
        Integer :: dx, dy
        Double precision :: x, y, z, z00, z01, z10, z11
        Double precision :: r, g, b
        Double precision, allocatable :: dfield(:,:,:)
        Double precision, allocatable :: rfieldx(:,:), rfieldy(:,:)

        mx = nint(log(dble(FIELD_SIZE_X))/log(2.0d0))
        my = nint(log(dble(FIELD_SIZE_Y))/log(2.0d0))

        rpx = 2**(mx-n) + 1
        rpy = 2**(my-n) + 1

        allocate(dfield(FIELD_SIZE_X, FIELD_SIZE_Y, FIELD_SIZE_Z))
        allocate(rfieldx(rpx, rpy))
        allocate(rfieldy(rpx, rpy))

        dx = 2**n
        dy = 2**n

        Do i=1, rpx
            Do j=1, rpy
                call random_number(rfieldx(i,j))
                call random_number(rfieldy(i,j))
                rfieldx(i,j) = rfieldx(i,j) - 0.5d0
                rfieldy(i,j) = rfieldy(i,j) - 0.5d0
            end Do
        end Do

        Do i=1, (FIELD_SIZE_X - 1)
            mx = i/dx
            Do j=1, (FIELD_SIZE_Z - 1)
                my = j/dy

                x = dble(i - dx*(mx + 1))
                y = dble(j - dy*(my + 1))
                z11 = x*rfieldx(mx + 2, my + 2) + y*rfieldy(mx + 2, my + 2)

                x = dble(i - dx*mx)
                y = dble(j - dy*(my + 1))
                z01 = x*rfieldx(mx + 1, my + 2) + y*rfieldy(mx + 1, my + 2)

                x = dble(i - dx*(mx + 1))
                y = dble(j - dy*my)
                z10 = x*rfieldx(mx + 2, my + 1) + y*rfieldy(mx + 2, my + 1)

                x = dble(i - dx*mx)
                y = dble(j - dy*my)
                z00 = x*rfieldx(mx + 1, my + 1) + y*rfieldy(mx + 1, my + 1)

                x = x/dble(dx)
                y = y/dble(dy)

                x = 6.0d0*x**5 - 15.0d0*x**4 + 10.0d0*x**3
                y = 6.0d0*y**5 - 15.0d0*y**4 + 10.0d0*y**3

                !x = 1.5d0*x
                !y = 1.5d0*y

                z = (1.0d0 - x)*(1.0d0 - y)*z00
                z = z + (1.0d0 - x)*y*z01
                z = z + x*(1.0d0 - y)*z10
                z = z + x*y*z11

                If((nint(z) + FIELD_CENTER_Y)<=FIELD_CENTER_Y*0.94d0) then
                    r = 0.110d0;
                    g = 0.020d0;
                    b = 1.0d0;
                else if(FIELD_CENTER_Y*0.94d0 < (nint(z) + FIELD_CENTER_Y) .and. (nint(z) + FIELD_CENTER_Y) <= FIELD_CENTER_Y) then
                    r = 0.522d0;
                    g = 0.478d0;
                    b = 0.235d0;
                else
                    r = 0.133d0;
                    g = 0.765d0;
                    b = 0.314d0;
                end If
                 
                FIELD(i,(nint(z) + FIELD_SIZE_Y/2 - FIELD_CENTER_Y),j)%wall = 1
                FIELD(i,(nint(z) + FIELD_SIZE_Y/2 - FIELD_CENTER_Y),j)%cr = r
                FIELD(i,(nint(z) + FIELD_SIZE_Y/2 - FIELD_CENTER_Y),j)%cg = g
                FIELD(i,(nint(z) + FIELD_SIZE_Y/2 - FIELD_CENTER_Y),j)%cb = b
                
            end Do
        end Do

    end subroutine create_random_field

    subroutine create_flat_field()
        Integer :: i, j 

        Do i=1, FIELD_SIZE_X
           Do j=1, FIELD_SIZE_Z
              FIELD(i, FIELD_CENTER_Y, j)%wall = 1
              FIELD(i, FIELD_CENTER_Y, j)%cr = 0.5d0
              FIELD(i, FIELD_CENTER_Y, j)%cg = 0.5d0
              FIELD(i, FIELD_CENTER_Y, j)%cb = 0.5d0
           end Do
        end Do

    end subroutine create_flat_field

    subroutine create_maze_field()
      Integer :: tryn
      Integer :: i, j, tmp, mc, bmc
      Integer :: xi, sxi, exi, nxi, nnxi, zi, szi, ezi, nzi, nnzi
      Integer :: msxi, mexi, mszi, mezi
      Integer :: bmsxi, bmexi, bmszi, bmezi
      Integer :: indn, flag, ff, p, pn, c
      Integer :: ind4(4)
      Integer, allocatable :: dfield(:, :)
      Integer, allocatable :: cfield(:, :)
      Integer, allocatable :: bdfield(:, :)
      Integer, allocatable :: bcfield(:, :)
      Integer, allocatable :: ind(:, :)
      Double precision :: rx, rz, ri
      Integer :: d, dmax

      allocate(dfield(FIELD_SIZE_X, FIELD_SIZE_Z))
      allocate(cfield(FIELD_SIZE_X, FIELD_SIZE_Z))
      allocate(bdfield(FIELD_SIZE_X, FIELD_SIZE_Z))
      allocate(bcfield(FIELD_SIZE_X, FIELD_SIZE_Z))

      nxi = 0
      nzi = 0
      nnxi = 0
      nnzi = 0

      sxi = 3
      szi = 3
      exi = FIELD_SIZE_X-2
      ezi = FIELD_SIZE_Z-2

      if(mod(FIELD_SIZE_X, 2)==0) then
         exi = FIELD_SIZE_X - 3
      end if
      if(mod(FIELD_SIZE_Z, 2)==0) then
         ezi = FIELD_SIZE_Z - 3
      end if

      indn = (exi-sxi+1)*(ezi-szi+1)
      allocate(ind(indn, 2))

      dmax = -1
      do tryn=1, 1000
         
         dfield(:, :) = 1
         dfield(:, 1) = 0
         dfield(:, FIELD_SIZE_Z) = 0
         dfield(1, :) = 0
         dfield(FIELD_SIZE_X, :) = 0

         if(mod(FIELD_SIZE_X, 2)==0) then
            dfield(FIELD_SIZE_X-1, :) = 0
         end if
         if(mod(FIELD_SIZE_Z, 2)==0) then
            dfield(:, FIELD_SIZE_Z-1) = 0
         end if

         cfield(:, :) = 0 
         ind(:, :) = -1
 
         call random_number(rx)
         call random_number(rz)

         xi = exi
         zi = szi
         
         msxi = xi
         mszi = zi
         
         ind(1, 1) = xi
         ind(1, 2) = zi
         pn = 2
         flag = 1
         ff   = 1
         mc = 0
         do while( .TRUE. )
            if(flag==1) then
               dfield(xi, zi) = 0
               if(ff==1) then
                  cfield(xi, zi) = 1
                  mexi = xi
                  mezi = zi
                  mc = mc + 1
               end if
            else
               ff = 0
               if(pn==1) then
                  exit
               end if
               call random_number(ri)
               p = nint(1.0d0 + dble(pn-2)*ri)
               xi = ind(p, 1)
               zi = ind(p, 2)
               !write(*, *) pn, p, xi, zi
               ind(p:indn-1, :) = ind(p+1:, :)
               pn = pn - 1
            end if
         
            ind4(1) = 1
            ind4(2) = 2
            ind4(3) = 3
            ind4(4) = 4
            do i=4, 2, -1
               call random_number(ri)
               j = nint(1.0d0 + 3.0d0*ri)
               tmp     = ind4(i)
               ind4(i) = ind4(j)
               ind4(j) = tmp
            end do
         
            flag = 0
            do i=1, 4
               if     (ind4(i)==1) then
                  nxi  = xi
                  nzi  = zi  + 1
                  nnxi = nxi
                  nnzi = nzi + 1
               else if(ind4(i)==2) then
                  nxi  = xi  + 1
                  nzi  = zi
                  nnxi = nxi + 1
                  nnzi = nzi
               else if(ind4(i)==3) then
                  nxi  = xi
                  nzi  = zi  - 1
                  nnxi = nxi
                  nnzi = nzi - 1
               else if(ind4(i)==4) then
                  nxi  = xi  - 1
                  nzi  = zi
                  nnxi = nxi - 1
                  nnzi = nzi
               end if

               c = dfield(nxi, nzi) + dfield(nnxi, nnzi)
               if(c==2) then
                  flag = 1
                  dfield(nxi, nzi) = 0
                  if(ff==1) then
                     cfield(nxi, nzi) = 1
                  end if
                  xi         = nnxi
                  zi         = nnzi
                  ind(pn, 1) = nnxi
                  ind(pn, 2) = nnzi
                  pn = pn + 1
                  exit
               end if
            end do
                       
         end do


         d = (mexi-msxi)**2 + (mezi-mszi)**2
         write(*, *) tryn, dmax, d, msxi, mszi, mexi, mezi
         if(dmax<d) then
            bmsxi = msxi
            bmszi = mszi
            bmexi = mexi
            bmezi = mezi
            bmc   = mc
            dmax  = d
            bdfield(:, :) = dfield(:, :)
            bcfield(:, :) = cfield(:, :)
         end if
         
      end do

      dfield(:, :) = bdfield(:, :)
      cfield(:, :) = bcfield(:, :)

      dfield(:, FIELD_SIZE_Z) = 1
      dfield(1, :) = 1
      dfield(:, 1) = 1
      dfield(FIELD_SIZE_X, :) = 1
      if(mod(FIELD_SIZE_X, 2)==0) then
         dfield(FIELD_SIZE_X-1, :) = 1
      end if
      if(mod(FIELD_SIZE_Z, 2)==0) then
         dfield(:, FIELD_SIZE_Z-1) = 1
      end if
      
      Do i=1, FIELD_SIZE_X
         Do j=1, FIELD_SIZE_Z
            FIELD(i, FIELD_CENTER_Y, j)%wall = 1
            if(cfield(i, j)==1) then
               FIELD(i, FIELD_CENTER_Y, j)%cr = 250.d0/255.0d0
               FIELD(i, FIELD_CENTER_Y, j)%cg = 128.d0/255.0d0
               FIELD(i, FIELD_CENTER_Y, j)%cb = 114.d0/255.0d0
            else
               FIELD(i, FIELD_CENTER_Y, j)%cr = 0.25d0
               FIELD(i, FIELD_CENTER_Y, j)%cg = 0.25d0
               FIELD(i, FIELD_CENTER_Y, j)%cb = 0.25d0
            end if              
            if(dfield(i, j)==1) then
               FIELD(i, FIELD_CENTER_Y:FIELD_CENTER_Y+5, j)%wall = 1
               FIELD(i, FIELD_CENTER_Y:FIELD_CENTER_Y+5, j)%cr = 0.5d0
               FIELD(i, FIELD_CENTER_Y:FIELD_CENTER_Y+5, j)%cg = 0.5d0
               FIELD(i, FIELD_CENTER_Y:FIELD_CENTER_Y+5, j)%cb = 0.5d0
            end if
         end Do
      end Do


      FIELD(bmsxi, FIELD_CENTER_Y, bmszi)%cr = 0.8d0
      FIELD(bmsxi, FIELD_CENTER_Y, bmszi)%cg = 0.1d0
      FIELD(bmsxi, FIELD_CENTER_Y, bmszi)%cb = 0.1d0
      
      FIELD(bmexi, FIELD_CENTER_Y, bmezi)%cr = 0.8d0
      FIELD(bmexi, FIELD_CENTER_Y, bmezi)%cg = 0.1d0
      FIELD(bmexi, FIELD_CENTER_Y, bmezi)%cb = 0.1d0

      write(*, *) "start =", bmsxi, bmszi
      write(*, *) "end   =", bmexi, bmezi
      write(*, *) "count =", bmc
        

    end subroutine create_maze_field


    subroutine create_maze_field_w()
      Integer :: tryn
      Integer :: i, j, tmp, mc, bmc
      Integer :: xi, sxi, exi, nxi, nnxi, zi, szi, ezi, nzi, nnzi
      Integer :: msxi, mexi, mszi, mezi
      Integer :: bmsxi, bmexi, bmszi, bmezi
      Integer :: indn, flag, ff, p, pn, c
      Integer :: ind4(4)
      Integer :: FIELD_SIZE_XH, FIELD_SIZE_ZH
      Integer, allocatable :: dfield(:, :)
      Integer, allocatable :: cfield(:, :)
      Integer, allocatable :: bdfield(:, :)
      Integer, allocatable :: bcfield(:, :)
      Integer, allocatable :: ind(:, :)
      Double precision :: rx, rz, ri
      Integer :: d, dmax

      FIELD_SIZE_XH = FIELD_SIZE_X/2
      FIELD_SIZE_ZH = FIELD_SIZE_Z/2
      
      allocate(dfield(FIELD_SIZE_XH, FIELD_SIZE_ZH))
      allocate(cfield(FIELD_SIZE_XH, FIELD_SIZE_ZH))
      allocate(bdfield(FIELD_SIZE_XH, FIELD_SIZE_ZH))
      allocate(bcfield(FIELD_SIZE_XH, FIELD_SIZE_ZH))

      nxi = 0
      nzi = 0
      nnxi = 0
      nnzi = 0
      bmsxi = 0
      bmszi = 0
      bmexi = 0
      bmezi = 0
      
      sxi = 3
      szi = 3
      exi = FIELD_SIZE_XH-2
      ezi = FIELD_SIZE_ZH-2

      if(mod(FIELD_SIZE_XH, 2)==0) then
         exi = FIELD_SIZE_XH - 3
      end if
      if(mod(FIELD_SIZE_ZH, 2)==0) then
         ezi = FIELD_SIZE_ZH - 3
      end if

      indn = (exi-sxi+1)*(ezi-szi+1)
      allocate(ind(indn, 2))

      dmax = -1
      do tryn=1, 1000
         
         dfield(:, :) = 1
         dfield(:, 1) = 0
         dfield(:, FIELD_SIZE_ZH) = 0
         dfield(1, :) = 0
         dfield(FIELD_SIZE_XH, :) = 0

         if(mod(FIELD_SIZE_XH, 2)==0) then
            dfield(FIELD_SIZE_XH-1, :) = 0
         end if
         if(mod(FIELD_SIZE_ZH, 2)==0) then
            dfield(:, FIELD_SIZE_ZH-1) = 0
         end if

         cfield(:, :) = 0 
         ind(:, :) = -1
 
         call random_number(rx)
         call random_number(rz)

         xi = exi
         zi = szi
         
         msxi = xi
         mszi = zi
         
         ind(1, 1) = xi
         ind(1, 2) = zi
         pn = 2
         flag = 1
         ff   = 1
         mc = 0
         do while( .TRUE. )
            if(flag==1) then
               dfield(xi, zi) = 0
               if(ff==1) then
                  cfield(xi, zi) = 1
                  mexi = xi
                  mezi = zi
                  mc = mc + 1
               end if
            else
               ff = 0
               if(pn==1) then
                  exit
               end if
               call random_number(ri)
               p = nint(1.0d0 + dble(pn-2)*ri)
               xi = ind(p, 1)
               zi = ind(p, 2)
               ind(p:indn-1, :) = ind(p+1:, :)
               pn = pn - 1
            end if
         
            ind4(1) = 1
            ind4(2) = 2
            ind4(3) = 3
            ind4(4) = 4
            do i=4, 2, -1
               call random_number(ri)
               j = nint(1.0d0 + 3.0d0*ri)
               tmp     = ind4(i)
               ind4(i) = ind4(j)
               ind4(j) = tmp
            end do
         
            flag = 0
            do i=1, 4
               if     (ind4(i)==1) then
                  nxi  = xi
                  nzi  = zi  + 1
                  nnxi = nxi
                  nnzi = nzi + 1
               else if(ind4(i)==2) then
                  nxi  = xi  + 1
                  nzi  = zi
                  nnxi = nxi + 1
                  nnzi = nzi
               else if(ind4(i)==3) then
                  nxi  = xi
                  nzi  = zi  - 1
                  nnxi = nxi
                  nnzi = nzi - 1
               else if(ind4(i)==4) then
                  nxi  = xi  - 1
                  nzi  = zi
                  nnxi = nxi - 1
                  nnzi = nzi
               end if

               c = dfield(nxi, nzi) + dfield(nnxi, nnzi)
               if(c==2) then
                  flag = 1
                  dfield(nxi, nzi) = 0
                  if(ff==1) then
                     cfield(nxi, nzi) = 1
                  end if
                  xi         = nnxi
                  zi         = nnzi
                  ind(pn, 1) = nnxi
                  ind(pn, 2) = nnzi
                  pn = pn + 1
                  exit
               end if
            end do
                       
         end do


         d = (mexi-msxi)**2 + (mezi-mszi)**2
         write(*, *) tryn, dmax, d, msxi, mszi, mexi, mezi
         if(dmax<d) then
            bmsxi = msxi
            bmszi = mszi
            bmexi = mexi
            bmezi = mezi
            bmc   = mc
            dmax  = d
            bdfield(:, :) = dfield(:, :)
            bcfield(:, :) = cfield(:, :)
         end if
         
      end do

      dfield(:, :) = bdfield(:, :)
      cfield(:, :) = bcfield(:, :)

      dfield(:, FIELD_SIZE_ZH-1:FIELD_SIZE_ZH) = 1
      dfield(1:2, :) = 1
      dfield(:, 1:2) = 1
      dfield(FIELD_SIZE_XH-1:FIELD_SIZE_XH, :) = 1
      if(mod(FIELD_SIZE_XH, 2)==0) then
         dfield(FIELD_SIZE_XH-1, :) = 1
      end if
      if(mod(FIELD_SIZE_ZH, 2)==0) then
         dfield(:, FIELD_SIZE_ZH-1) = 1
      end if
      
      Do i=1, FIELD_SIZE_XH-1
         Do j=1, FIELD_SIZE_ZH-1
            FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y, 2*j-1+1:2*j+1)%wall = 1
            if(cfield(i, j)==1) then
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y, 2*j-1+1:2*j+1)%cr = 250.d0/255.0d0
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y, 2*j-1+1:2*j+1)%cg = 128.d0/255.0d0
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y, 2*j-1+1:2*j+1)%cb = 114.d0/255.0d0
               !FIELD(2*i  , FIELD_CENTER_Y, 2*j  )%cr = 250.d0/255.0d0
               !FIELD(2*i  , FIELD_CENTER_Y, 2*j  )%cg = 128.d0/255.0d0
               !FIELD(2*i  , FIELD_CENTER_Y, 2*j  )%cb = 114.d0/255.0d0
            else
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y, 2*j-1+1:2*j+1)%cr = 0.25d0
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y, 2*j-1+1:2*j+1)%cg = 0.25d0
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y, 2*j-1+1:2*j+1)%cb = 0.25d0
               !FIELD(2*i  , FIELD_CENTER_Y, 2*j  )%cr = 0.25d0
               !FIELD(2*i  , FIELD_CENTER_Y, 2*j  )%cg = 0.25d0
               !FIELD(2*i  , FIELD_CENTER_Y, 2*j  )%cb = 0.25d0
            end if              
            if(dfield(i, j)==1) then
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y:FIELD_CENTER_Y+5, 2*j-1+1:2*j+1)%wall = 1
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y:FIELD_CENTER_Y+5, 2*j-1+1:2*j+1)%cr = 0.5d0
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y:FIELD_CENTER_Y+5, 2*j-1+1:2*j+1)%cg = 0.5d0
               FIELD(2*i-1+1:2*i+1, FIELD_CENTER_Y:FIELD_CENTER_Y+5, 2*j-1+1:2*j+1)%cb = 0.5d0
               !FIELD(2*i  , FIELD_CENTER_Y:FIELD_CENTER_Y+5, 2*j  )%wall = 1
               !FIELD(2*i  , FIELD_CENTER_Y:FIELD_CENTER_Y+5, 2*j  )%cr = 0.5d0
               !FIELD(2*i  , FIELD_CENTER_Y:FIELD_CENTER_Y+5, 2*j  )%cg = 0.5d0
               !FIELD(2*i  , FIELD_CENTER_Y:FIELD_CENTER_Y+5, 2*j  )%cb = 0.5d0
            end if
         end Do
      end Do


      FIELD(2*bmsxi-1+1:2*bmsxi+1, FIELD_CENTER_Y, 2*bmszi-1+1:2*bmszi+1)%cr = 0.8d0
      FIELD(2*bmsxi-1+1:2*bmsxi+1, FIELD_CENTER_Y, 2*bmszi-1+1:2*bmszi+1)%cg = 0.1d0
      FIELD(2*bmsxi-1+1:2*bmsxi+1, FIELD_CENTER_Y, 2*bmszi-1+1:2*bmszi+1)%cb = 0.1d0
      FIELD(2*bmexi-1+1:2*bmexi+1, FIELD_CENTER_Y, 2*bmezi-1+1:2*bmezi+1)%cr = 0.8d0
      FIELD(2*bmexi-1+1:2*bmexi+1, FIELD_CENTER_Y, 2*bmezi-1+1:2*bmezi+1)%cg = 0.1d0
      FIELD(2*bmexi-1+1:2*bmexi+1, FIELD_CENTER_Y, 2*bmezi-1+1:2*bmezi+1)%cb = 0.1d0

      write(*, *) "start =", ((2*bmsxi-1+1)+(2*bmsxi+1))/2, ((2*bmszi-1+1)+(2*bmszi+1))/2
      write(*, *) "end   =", ((2*bmexi-1+1)+(2*bmexi+1))/2, ((2*bmezi-1+1)+(2*bmezi+1))/2
      write(*, *) "count =", bmc
        

    end subroutine create_maze_field_w

    
    subroutine make_field_list()
        Integer :: i, j, k, c
        
        c = 0
        Do i=1, FIELD_SIZE_X
            Do j=1, FIELD_SIZE_Y
                Do k=1, FIELD_SIZE_Z
                    If(1<=FIELD(i,j,k)%wall) then
                        c = c + 1
                    end If
                end Do
            end Do
        end Do
        allocate(block_list(c))
        block_list_length = c
        c = 1
        Do i=1, FIELD_SIZE_X
            Do j=1, FIELD_SIZE_Y
                Do k=1, FIELD_SIZE_Z
                    If(1<=FIELD(i,j,k)%wall) then
                        block_list(c) = FIELD(i,j,k)
                        c = c + 1
                    end If
                end Do
            end Do
        end Do
    end subroutine make_field_list

    subroutine read_setting() bind(c)
      Character(len=7) :: file_name = './input'
      Integer :: fp = 12
      namelist/world/WORLD_STEP
      namelist/world/OUT_INTERVAL_STEP
      namelist/world/WORLD_DT, WORLD_G 
      namelist/world/BLOCK_EMD_FORCE_K, BLOCK_EMD_FORCE_R
      namelist/world/CELL_EMD_FORCE_K, CELL_EMD_FORCE_R
      namelist/world/AIR_RESISTANCE_K
      namelist/world/FIELD_INIT_FLAG
      namelist/world/FIELD_ADJUST_FLAG
      namelist/world/SUN_INIT_FLAG
      namelist/world/DISTANCE2SUN
      namelist/world/SUM_LIMIT_NUM
      namelist/world/SUN_CYCLE
      namelist/world/SUN_AMP
      namelist/world/FIELD_SIZE_X, FIELD_SIZE_Y, FIELD_SIZE_Z
      namelist/world/FIELD_CENTER_X, FIELD_CENTER_Y, FIELD_CENTER_Z
      namelist/world/DEPTH_LIMIT, HEIGHT_LIMIT
      namelist/world/LOG_FLAG
      namelist/world/CONTACT_AREA_CORR
      
      namelist/cell/CELLS_INIT_FLAG
      namelist/cell/EVERY_STEP_COST_UPDATE_FLAG
      namelist/cell/EVERY_STEP_COST_E
      namelist/cell/EVERY_STEP_COST_A
      namelist/cell/EVERY_STEP_COST_D
      namelist/cell/EVERY_STEP_COST_M
      namelist/cell/TARGET_CELLS_NUM
      namelist/cell/ALONE_COST_E_COEFF
      namelist/cell/MUTATION_RATE_FLAG
      namelist/cell/MUTATION_DIVISION_RATE
      namelist/cell/MUTATION_RATE_AMP
      namelist/cell/MUTATION_RATE_THR
      namelist/cell/MUTATION_CENTER
      namelist/cell/MUTATION_RATE_T
      namelist/cell/MUTATION_COEFF4EXPANSION
      namelist/cell/NUMBER_OF_CELL
      namelist/cell/ABS_KEEP_NUM
      namelist/cell/ECEOL_E2L
      namelist/cell/ECEOL_L2E
      namelist/cell/ECEOLR
      namelist/cell/ECEOLG
      namelist/cell/ECEOLB
      namelist/cell/ECEOE
      namelist/cell/ENERGY_TRANSIT_FLAG
      namelist/cell/ENERGY_TRANSIT_MIN
      namelist/cell/ENERGY_TRANSIT_C
      namelist/cell/GENE_COST_C
      namelist/cell/CONN_COST_C
      namelist/cell/BDRC
      namelist/cell/MIN_R
      namelist/cell/TRANS_INTERVAL_STEP, RESET_INTERVAL_STEP
      namelist/cell/SPRING_LIMIT_C, SPRING_CATCH_C, SPRING_BREAK_C
      namelist/cell/THRESHOLD_EAT, THRESHOLD_FUSION, THRESHOLD_LIGHT
      namelist/cell/RATE_OF_VARI_S
      namelist/cell/RATE_OF_VARI_L, RANGE_OF_VARI_L_D
      
      namelist/system/RANDOM_SEED_FLAG
      namelist/system/TRACE_FLAG
      namelist/system/FIX_FLAG
      namelist/system/FRAGMENT_FLAG
      namelist/system/FRAGMENT_INTERVAL_STEP
      
      open(fp, file=file_name)
      read(fp, nml=world)
      read(fp, nml=cell)
      read(fp, nml=system)
      close(fp)

      write(*, nml=world)
      write(*, nml=cell)
      write(*, nml=system)
      
      ORG_EVERY_STEP_COST_E = EVERY_STEP_COST_E
      
    end subroutine read_setting

    subroutine trace_init()
      Integer :: fp=12
      Integer :: i, id
      Character(len=128) :: filename

      open(fp, file="./trace.ini", status="old")
      read(fp, *) TRACE_NUM
      allocate(TRACE_LIST(TRACE_NUM))
      read(fp, *) TRACE_LIST(:)
      close(fp)

      Do i=1, TRACE_NUM
         id = TRACE_LIST(i)

         open(fp, file="./trace_free.ini", status="replace")
         close(fp)
         
         write(filename, '("trace_", i7.7, "_in.dat")') id
         open(fp, file=trim(filename), status="replace")
         close(fp)

         write(filename, '("trace_", i7.7, "_h.dat")') id
         open(fp, file=trim(filename), status="replace")
         close(fp)

         write(filename, '("trace_", i7.7, "_out.dat")') id
         open(fp, file=trim(filename), status="replace")
         close(fp)

      end Do
      
    end subroutine trace_init

    subroutine fix_init()
      Integer :: fp=12
      Integer :: i, id

      open(fp, file="./fix.ini", status="old")
      read(fp, *) FIX_NUM
      allocate(FIX_LIST(FIX_NUM))
      allocate(FIX_ENERGY(FIX_NUM))
      read(fp, *) FIX_LIST(:)
      close(fp)

      do i=1, FIX_NUM
         id = FIX_LIST(i)
         FIX_ENERGY(i) = CELLS(id)%E
      end do
      
    end subroutine fix_init

    subroutine fragment_init()
      Integer :: fp=12
      Integer :: i, j, n
      Integer :: id, CALC_CLn
      Integer, allocatable :: fragment_count(:)
      Character(len=FRAGMENT_MAX_LENGTH) :: fragment
      Character(len=128) :: filename
      
      open(fp, file="./fragment.ini", status="old")
      read(fp, *) FRAGMENT_NUM
      allocate(FRAGMENT_LIST(FRAGMENT_NUM, FRAGMENT_MAX_LENGTH))
      
      FRAGMENT_LIST(:, :) = 0

      do i=1, FRAGMENT_NUM
      
         read(fp, *) fragment
         do j=1, FRAGMENT_MAX_LENGTH
            n = index(CODES, fragment(j:j))
            FRAGMENT_LIST(i, j) = n
         end do

         write(*, *) fragment, FRAGMENT_LIST(i, :)

      end do
      
      close(fp)

      If(FRAGMENT_FLAG==1) then
      
         allocate(fragment_count(FRAGMENT_NUM))
         fragment_count(:) = 0
         Do CALC_CLn=1, CALC_CLL
            id = CALC_CL(CALC_CLn)
            If(CELLS(id)%INFO_TRANS_F==0) then
               Do i=1, FRAGMENT_NUM
                  fragment(:) = ""
                  Do j=1, FRAGMENT_MAX_LENGTH
                     n = FRAGMENT_LIST(i, j)
                     If(n==0) then
                        exit
                     end If
                     fragment(j:j) = CODES(n:n)
                  end Do
                  If(index(CELLS(id)%book, trim(adjustl(fragment)))/=0) then
                     fragment_count(i) = fragment_count(i) + 1
                  end If
               end Do
            end If
         end Do
         
         Do i=1, FRAGMENT_NUM
            write (filename, '("fragment", i4.4, ".d")') i
            open(fp, file=trim(filename), status="replace")
            write(fp, *) WORLD_STEP, fragment_count(i)
            close(fp)
         end Do
         
         deallocate(fragment_count)

      else If(FRAGMENT_FLAG==2) then

         open(fp, file="fragment.d", status="replace")

         write(fp, *) "WORLD_STEP=", WORLD_STEP 
         Do CALC_CLn=1, CALC_CLL
            id = CALC_CL(CALC_CLn)
            If(CELLS(id)%INFO_TRANS_F==0) then
               Do i=1, FRAGMENT_NUM
                  fragment(:) = ""
                  Do j=1, FRAGMENT_MAX_LENGTH
                     n = FRAGMENT_LIST(i, j)
                     If(n==0) then
                        exit
                     end If
                     fragment(j:j) = CODES(n:n)
                  end Do
                  If(index(CELLS(id)%book, trim(adjustl(fragment)))/=0) then
                     write(fp, *) id, i, CELLS(id)%x, CELLS(id)%y, CELLS(id)%z, CELLS(id)%E
                  end If
               end Do
            end If
         end Do
         
         close(fp)
         
      end If
            
    end subroutine fragment_init
    
    subroutine praparat_init()  bind(c)
        Integer :: id, i, fp=12

        integer :: seedsize
        integer, allocatable :: seed(:)

        call read_setting()
        
        if(RANDOM_SEED_FLAG==0) then
           
           write(*, *) 'Seed dose not set as random'
           
        else if(RANDOM_SEED_FLAG==1) then
           
           write(*, *) 'Read random seed'
           call random_seed(size=seedsize)
           allocate(seed(seedsize))
           seed(:) = 0
           open(fp, file="./seed.ini", status="old")
           do i = 1, seedsize
              read(fp, *) seed(i)
           end do
           close(fp)
           call random_seed(put=seed(:))
           
        else if(RANDOM_SEED_FLAG==2) then

           write(*, *) 'Seed set as random'
           call random_seed(size=seedsize)
           allocate(seed(seedsize))
           do i = 1, seedsize
              call system_clock(count=seed(i))
           end do
           call random_seed(put=seed(:))
           open(fp, file="./seed.ini", status='replace')
           do i = 1, seedsize
              write(fp, *) seed(i)
           end do
           close(fp)

        end if

        CONN_CLL_MAX = (NOCC+2)*NUMBER_OF_CELL 
        allocate(CONN_CL(CONN_CLL_MAX,2,2))
        allocate(BACK_CONN_CL(CONN_CLL_MAX,2,2))
        allocate(CALC_CL(NUMBER_OF_CELL))
        allocate(BACK_CALC_CL(NUMBER_OF_CELL))
        allocate(NOT_CALC_CL(NUMBER_OF_CELL))
        allocate(CELLS(NUMBER_OF_CELL))
        allocate(AGE_COUNT(TRANS_INTERVAL_STEP))
        allocate(EVERY_STEP_COST_LIST(NOCC+1))

        write(*, *) 'call field_init()'
        call field_init()
        write(*, *) 'field_init() done'

        Do i=1, NUMBER_OF_CELL
            call free_cell(i)
        end Do

        If(CELLS_INIT_FLAG==0) then
            write(*, *) "call read_cells('./cells/data')"
            call read_cells('./cells/data')
            write(*, *) "read_cells('./cells/data') done"
            if(SUN_INIT_FLAG==1) then
               call sun_init(1)
            end if
        else If(CELLS_INIT_FLAG==1) then
            write(*, *) "call read_cells_from_one('./cells_file')"
            call read_cells_from_one('./cells_file')
            write(*, *) "read_cells_from_one('./cells_file') done"
            if(SUN_INIT_FLAG==1) then
               call sun_init(1)
            end if
        else If(CELLS_INIT_FLAG==2) then
            if(SUN_INIT_FLAG==1) then
               call sun_init(1)
            end if
            call create_tetrahedra_cell(2, -1.0d0, FIELD_TOP+5.0d0, -1.0d0, 10000.0d0) 
         else If(CELLS_INIT_FLAG==3) then
            write(*, *) "call read_cells_from_one('./cells_file')"
            call read_cells_from_one('./cells_file')
            write(*, *) "read_cells_from_one('./cells_file') done"
            
            if(SUN_INIT_FLAG==1) then
               call sun_init(1)
            end if

            Do i=2, NUMBER_OF_CELL
               if(CELLS(i)%exist==0) then
                  id = i
                  exit
               end If
            end Do

            call create_tetrahedra_cell(id, -1.0d0, FIELD_TOP+20.0d0, -1.0d0, 10000.0d0) 
         else
            !
         end If

        if(TRACE_FLAG==1) then
           call trace_init()
        end if

        if(0<FIX_FLAG) then
           call fix_init()
        end if
        
        call init_calculation_cell_list_first()
        call init_CONN_CL()
        call init_calculation_cell_list_second()
        call update_CONN_CL()

        if(0<FRAGMENT_FLAG) then
           call fragment_init()
        end if

    end subroutine praparat_init

    subroutine write_field(filename)
        Character(len=*) :: filename
        Integer :: i, j, k, fp = 12
        open(fp, file=filename, status='replace')
        write(fp,*) FIELD_SIZE_X, FIELD_SIZE_Y, FIELD_SIZE_Z
        write(fp,*) CELL_CAPACITY_OF_BLOCK
        Do i=1, FIELD_SIZE_X
            Do j=1, FIELD_SIZE_Y
                Do k=1, FIELD_SIZE_Z
                    write(fp, "(*(G0, ' '))") FIELD(i,j,k)%wall
                    write(fp, "(*(G0, ' '))") FIELD(i,j,k)%NOC
                    write(fp, "(*(G0, ' '))") FIELD(i,j,k)%IOC(:)
                    write(fp, "(*(G0, ' '))") FIELD(i,j,k)%x, FIELD(i,j,k)%y, FIELD(i,j,k)%z
                    write(fp, "(*(G0, ' '))") FIELD(i,j,k)%cr, FIELD(i,j,k)%cg, FIELD(i,j,k)%cb
                    write(fp, "(*(G0, ' '))") FIELD(i,j,k)%ar, FIELD(i,j,k)%ag, FIELD(i,j,k)%ab
                end Do
            end Do
        end Do
        close(fp)
    end subroutine write_field

    subroutine read_field(filename)
      Character(len=*) :: filename
      Integer :: i, j, k, fp = 12
      Integer :: fsx, fsy, fsz, ccob
      open(fp, file=filename)
      read(fp,*) fsx, fsy, fsz
      read(fp,*) ccob

      FIELD_SIZE_X = fsx
      FIELD_SIZE_Y = fsy
      FIELD_SIZE_Z = fsz
      
      allocate(FIELD(FIELD_SIZE_X, FIELD_SIZE_Y, FIELD_SIZE_Z))
      
      FIELD(:, :, :)%wall = 0
      FIELD(:, :, :)%NOC = 0
      FIELD(:, :, :)%cr = 0.0d0
      FIELD(:, :, :)%cg = 0.0d0
      FIELD(:, :, :)%cb = 0.0d0
      FIELD(:, :, :)%ar = 0.5d0
      FIELD(:, :, :)%ag = 0.5d0
      FIELD(:, :, :)%ab = 0.5d0   
        
      Do i=1, FIELD_SIZE_X
         Do j=1, FIELD_SIZE_Y
            Do k=1, FIELD_SIZE_Z
               read(fp,*) FIELD(i,j,k)%wall
               read(fp,*) FIELD(i,j,k)%NOC
               read(fp,*) FIELD(i,j,k)%IOC(:)
               read(fp,*) FIELD(i,j,k)%x, FIELD(i,j,k)%y, FIELD(i,j,k)%z
               read(fp,*) FIELD(i,j,k)%cr, FIELD(i,j,k)%cg, FIELD(i,j,k)%cb
               read(fp,*) FIELD(i,j,k)%ar, FIELD(i,j,k)%ag, FIELD(i,j,k)%ab
            end Do
         end Do
      end Do
      
      close(fp)
      
    end subroutine read_field

    subroutine write_cells(basefilename)
        Character(len=*) :: basefilename
        Character(len=128) :: filename
        Integer :: id 
        Double precision :: time

        call timer('start', time)

        write(*, *) 'MKDIR', dir_name(basefilename)
        call makedirs(dir_name(basefilename))
        Do id=1, NUMBER_OF_CELL
            write (filename, '(i7.7, ".d")') id
            filename = basefilename//trim(adjustl(filename))
            call write_cell(trim(adjustl(filename)), id)
        end Do
        call tardirs(dir_name(basefilename))
        call removedirs(dir_name(basefilename))

        call timer('end', time)
        write(*,*) 'The time to write cells =', time

    end subroutine write_cells

    subroutine write_cell(filename, id)
        Character(len=*) :: filename
        Integer :: id, fp = 12
        open(fp, file=filename, status='replace')
        
        !Integer :: exist
        write(fp, "(*(G0, ' '))") CELLS(id)%exist

        !Character(len=BOOK_MAX_LENGTH) :: book
        write(fp, '(A)') '!'//trim(CELLS(id)%book)
        !Character(len=BOOKMARKER_LENGTH) :: bookmarker
        write(fp, '(A)') '!'//trim(CELLS(id)%bookmarker)
        !Character(len=BOOKMARKER_ADVANCE_LENGTH) :: bookmarker_advance
        write(fp, '(A)') '!'//trim(CELLS(id)%bookmarker_advance)

        !Double precision :: x, y, z
        write(fp, "(*(G0, ' '))") CELLS(id)%x, CELLS(id)%y, CELLS(id)%z
        !Integer :: nx, ny, nz
        write(fp, "(*(G0, ' '))") CELLS(id)%nx, CELLS(id)%ny, CELLS(id)%nz
        !Double precision :: vx, vy, vz
        write(fp, "(*(G0, ' '))") CELLS(id)%vx, CELLS(id)%vy, CELLS(id)%vz
        !Double precision :: fx, fy, fz
        write(fp, "(*(G0, ' '))") CELLS(id)%fx, CELLS(id)%fy, CELLS(id)%fz
        !Double precision :: cr, cg, cb
        write(fp, "(*(G0, ' '))") CELLS(id)%cr, CELLS(id)%cg, CELLS(id)%cb
        !Double precision :: ar, ag, ab
        write(fp, "(*(G0, ' '))") CELLS(id)%ar, CELLS(id)%ag, CELLS(id)%ab
        !Double precision :: IOLr  !Intensity Of Light r
        write(fp, "(*(G0, ' '))") CELLS(id)%IOLr
        !Double precision :: IOLg  !Intensity Of Light g
        write(fp, "(*(G0, ' '))") CELLS(id)%IOLg
        !Double precision :: IOLb  !Intensity Of Light b
        write(fp, "(*(G0, ' '))") CELLS(id)%IOLb
        !Double precision :: m
        write(fp, "(*(G0, ' '))") CELLS(id)%m
        !Double precision :: r
        write(fp, "(*(G0, ' '))") CELLS(id)%r
        !Double precision :: E
        write(fp, "(*(G0, ' '))") CELLS(id)%E
        !Double precision :: alpha
        write(fp, "(*(G0, ' '))") CELLS(id)%alpha

        !Integer :: ID
        write(fp, "(*(G0, ' '))") CELLS(id)%ID
        
        !Integer :: age
        write(fp, "(*(G0, ' '))") CELLS(id)%age

        !Double precision :: in_data(NOIU)
        write(fp, "(*(G0, ' '))") CELLS(id)%in_data(:)
        !Double precision :: out_data(NOOU)
        write(fp, "(*(G0, ' '))") CELLS(id)%out_data(:)
        !Double precision :: w1(NOHU,(NOIU+1))
        write(fp, "(*(G0, ' '))") CELLS(id)%w1(:,:)
        !Double precision :: w2(NOOU,(NOHU+1))
        write(fp, "(*(G0, ' '))") CELLS(id)%w2(:,:)

        !Double precision :: KOCC(NOCC)    !K Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%KOCC(:)
        !Double precision :: LOCC(NOCC)    !L Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%LOCC(:)
        !Double precision :: SOCC(NOCC)    !S Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%SOCC(:)

        !Integer :: CCF(NOCC)      !Connected Cell Flag
        write(fp, "(*(G0, ' '))") CELLS(id)%CCF(:)
        !Integer :: IOCC(NOCC)     !Id Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%IOCC(:)
        !Integer :: UIOCC(NOCC)    !Unit Id Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%UIOCC(:)

        !Integer :: HIT_BLOCK_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%HIT_BLOCK_AF
        !Integer :: HIT_CELL_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%HIT_CELL_AF
        !Integer :: SPRING_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%SPRING_AF
        !Integer :: MECHANICS_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%MECHANICS_AF

        !Integer :: EAT_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%EAT_AF
        !Integer :: FUSION_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%FUSION_AF
        !Integer :: LIGHT_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%LIGHT_AF

        !Integer :: INFO_TRANS_F
        write(fp, "(*(G0, ' '))") CELLS(id)%INFO_TRANS_F
        !Integer :: NEURAL_NETWORK_F
        write(fp, "(*(G0, ' '))") CELLS(id)%NEURAL_NETWORK_F

        !Integer :: WAIT_FOR_CONNECT_F
        write(fp, "(*(G0, ' '))") CELLS(id)%WAIT_FOR_CONNECT_F
        !Integer :: WAIT_FOR_CONNECT_UI
        write(fp, "(*(G0, ' '))") CELLS(id)%WAIT_FOR_CONNECT_UI
        !Integer :: WAIT_FOR_DISCONNECT_F
        write(fp, "(*(G0, ' '))") CELLS(id)%WAIT_FOR_DISCONNECT_F

        !Integer :: ALONE_F
        write(fp, "(*(G0, ' '))") CELLS(id)%ALONE_F
        close(fp)
    end subroutine write_cell

    subroutine read_cells(basefilename)
        Character(len=*) :: basefilename
        Character(len=128) :: filename
        Integer :: access, status
        Integer :: id 
        Do id=1, NUMBER_OF_CELL
            write (filename, '(i7.7, ".d")') id
            filename = basefilename//trim(filename)
            status = access(filename, " ")
            If(status==0) then
               call read_cell(filename, id)
            end If
         end Do
    end subroutine read_cells

    subroutine read_cell(filename, id)
        Character(len=*) :: filename
        Character(len=(BOOK_MAX_LENGTH+1)) :: book
        Character(len=(BOOKMARKER_LENGTH+1)) :: bookmarker
        Character(len=(BOOKMARKER_ADVANCE_LENGTH+1)) :: bookmarker_advance
        Integer :: id, fp = 12
        type(cell) :: Rcell
        open(fp, file=filename)

        !write(*, *) 'read cell'
        !write(*, *) '  filename=', filename
        !write(*, *) '        ID=', id

        !Integer :: exist
        !write(*, *) 'read exist'
        read(fp,*) Rcell%exist

        !Character(len=BOOK_MAX_LENGTH) :: book
        !write(*, *) 'read book'
        read(fp,'(A)') book
        Rcell%book(:) = book(2:)
        !If(len_trim(Rcell%book(:))/=0) then
        !   BOOK_FINITE_COUNT = BOOK_FINITE_COUNT + 1
        !end If

        !Character(len=BOOKMARKER_LENGTH) :: bookmarker
        !write(*, *) 'read bookmarker'
        read(fp,'(A)') bookmarker
        Rcell%bookmarker(:) = bookmarker(2:)

        !write(*, *) 'read bookmarker_advance'
        read(fp,'(A)') bookmarker_advance
        Rcell%bookmarker_advance(:) = bookmarker_advance(2:)
        !write(*, *) 'bookmarker_advance', bookmarker_advance
        !write(*, *) 'Rcell%bookmarker_advance', Rcell%bookmarker_advance

        !Double precision :: x, y, z
        !write(*, *) 'read x, y, z'
        read(fp,*) Rcell%x, Rcell%y, Rcell%z

        !Integer :: nx, ny, nz
        !write(*, *) 'read nx, ny, nz'
        read(fp,*) Rcell%nx, Rcell%ny, Rcell%nz

        !Double precision :: vx, vy, vz
        !write(*, *) 'read vx, vy, vz'
        read(fp,*) Rcell%vx, Rcell%vy, Rcell%vz

        !Double precision :: fx, fy, fz
        !write(*, *) 'read fx, fy, fz'
        read(fp,*) Rcell%fx, Rcell%fy, Rcell%fz

        !Double precision :: cr, cg, cb
        !write(*, *) 'read cr, cg, cb'
        read(fp,*) Rcell%cr, Rcell%cg, Rcell%cb

        !Double precision :: ar, ag, ab
        !write(*, *) 'read ar, ag, ab'
        read(fp,*) Rcell%ar, Rcell%ag, Rcell%ab

        !Double precision :: IOLr  !Intensity Of Light r
        !write(*, *) 'read IOLr  !Intensity Of Light r'
        read(fp,*) Rcell%IOLr

        !Double precision :: IOLg  !Intensity Of Light g
        !write(*, *) 'read IOLg  !Intensity Of Light g'
        read(fp,*) Rcell%IOLg

        !Double precision :: IOLb  !Intensity Of Light b
        !write(*, *) 'read IOLb  !Intensity Of Light b'
        read(fp,*) Rcell%IOLb

        !Double precision :: m
        !write(*, *) 'read m'
        read(fp,*) Rcell%m
        If(Rcell%m < MIN_M) then
           Rcell%m = MIN_M
        else If(MAX_M<Rcell%m) then
           Rcell%m = MAX_M
        end If
        !Double precision :: r
        !write(*, *) 'read r'
        read(fp,*) Rcell%r

        !Double precision :: E
        !write(*, *) 'read E'
        read(fp,*) Rcell%E

        !Double precision :: alpha
        !write(*, *) 'read alpha'
        read(fp,*) Rcell%alpha

        !Integer :: ID
        !write(*, *) 'read ID'
        read(fp,*) Rcell%ID

        !Integer :: age
        !write(*, *) 'read age'
        read(fp,*) Rcell%age

        !Double precision :: in_data(NOIU)
        !write(*, *) 'read in_data(NOIU)'
        read(fp,*) Rcell%in_data(:)

        !Double precision :: out_data(NOOU)
        !write(*, *) 'read out_data(NOOU)'
        read(fp,*) Rcell%out_data(:)

        !Double precision :: w1(NOHU,(NOIU+1))
        !write(*, *) 'read w1(NOHU,(NOIU+1))'
        read(fp,*) Rcell%w1(:,:)

        !Double precision :: w2(NOOU,(NOHU+1))
        !write(*, *) 'read w2(NOOU,(NOHU+1))'
        read(fp,*) Rcell%w2(:,:)

        !Double precision :: KOCC(NOCC)    !K Of Connected Cell
        !write(*, *) 'read KOCC(NOCC)    !K Of Connected Cell'
        read(fp,*) Rcell%KOCC(:)

        !Double precision :: LOCC(NOCC)    !L Of Connected Cell
        !write(*, *) 'read LOCC(NOCC)    !L Of Connected Cell'
        read(fp,*) Rcell%LOCC(:)

        !Double precision :: SOCC(NOCC)    !S Of Connected Cell
        !write(*, *) 'read SOCC(NOCC)    !S Of Connected Cell'
        read(fp,*) Rcell%SOCC(:)

        !Integer :: CCF(NOCC)      !Connected Cell Flag
        !write(*, *) 'read CCF(NOCC)      !Connected Cell Flag'
        read(fp,*) Rcell%CCF(:)

        !Integer :: IOCC(NOCC)     !Id Of Connected Cell
        !write(*, *) 'read IOCC(NOCC)     !Id Of Connected Cell'
        read(fp,*) Rcell%IOCC(:)

        !Integer :: UIOCC(NOCC)    !Unit Id Of Connected Cell
        !write(*, *) 'read UIOCC(NOCC)    !Unit Id Of Connected Cell'
        read(fp,*) Rcell%UIOCC(:)

        !Integer :: HIT_BLOCK_AF
        !write(*, *) 'read HIT_BLOCK_AF'
        read(fp,*) Rcell%HIT_BLOCK_AF

        !Integer :: HIT_CELL_AF
        !write(*, *) 'read HIT_CELL_AF'
        read(fp,*) Rcell%HIT_CELL_AF

        !Integer :: SPRING_AF
        !write(*, *) 'read SPRING_AF'
        read(fp,*) Rcell%SPRING_AF

        !Integer :: MECHANICS_AF
        !write(*, *) 'read MECHANICS_AF'
        read(fp,*) Rcell%MECHANICS_AF

        !Integer :: EAT_AF
        !write(*, *) 'read EAT_AF'
        read(fp,*) Rcell%EAT_AF

        !Integer :: FUSION_AF
        !write(*, *) 'read FUSION_AF'
        read(fp,*) Rcell%FUSION_AF

        !Integer :: LIGHT_AF
        !write(*, *) 'read LIGHT_AF'
        read(fp,*) Rcell%LIGHT_AF

        !Integer :: INFO_TRANS_F
        !write(*, *) 'read INFO_TRANS_F'
        read(fp,*) Rcell%INFO_TRANS_F

        !Integer :: NEURAL_NETWORK_F
        !write(*, *) 'read NEURAL_NETWORK_F'
        read(fp,*) Rcell%NEURAL_NETWORK_F

        !Integer :: WAIT_FOR_CONNECT_F
        !write(*, *) 'read WAIT_FOR_CONNECT_F'
        read(fp,*) Rcell%WAIT_FOR_CONNECT_F

        !Integer :: WAIT_FOR_CONNECT_UI
        !write(*, *) 'read WAIT_FOR_CONNECT_UI'
        read(fp,*) Rcell%WAIT_FOR_CONNECT_UI

        !Integer :: WAIT_FOR_DISCONNECT_F
        !write(*, *) 'read WAIT_FOR_DISCONNECT_F'
        read(fp,*) Rcell%WAIT_FOR_DISCONNECT_F

        !Integer :: ALONE_F
        !write(*, *) 'read ALONE_F'
        read(fp,*) Rcell%ALONE_F

        close(fp)
        If(id==0) then
            CELLS(Rcell%id) = Rcell
        else
            CELLS(id) = Rcell
        end If
    end subroutine read_cell

    subroutine write_cells_into_one(basefilename)
        Character(len=*) :: basefilename
        Integer :: id, fp = 120
        Double precision :: time

        call timer('start', time)

        open(fp, file=trim(adjustl(basefilename)), status='replace')

        write(fp, "(*(G0, ' '))") NUMBER_OF_CELL 
        
        Do id=1, NUMBER_OF_CELL
           call write_cell_into_one(fp, id)
        end Do

        close(fp)
        
        call tarfile(basefilename)
        call removefile(basefilename)

        call timer('end', time)
        write(*,*) 'The time to write cells into one file =', time

    end subroutine write_cells_into_one

    subroutine write_cell_into_one(fp, id)
        Integer :: fp, id
        
        !Integer :: exist
        write(fp, "(*(G0, ' '))") CELLS(id)%exist

        !Character(len=BOOK_MAX_LENGTH) :: book
        write(fp,'(A)') '!'//trim(CELLS(id)%book)
        !Character(len=BOOKMARKER_LENGTH) :: bookmarker
        write(fp,'(A)') '!'//trim(CELLS(id)%bookmarker)
        !Character(len=BOOKMARKER_ADVANCE_LENGTH) :: bookmarker_advance
        write(fp,'(A)') '!'//trim(CELLS(id)%bookmarker_advance)

        !Double precision :: x, y, z
        write(fp, "(*(G0, ' '))") CELLS(id)%x, CELLS(id)%y, CELLS(id)%z
        !Integer :: nx, ny, nz
        write(fp, "(*(G0, ' '))") CELLS(id)%nx, CELLS(id)%ny, CELLS(id)%nz
        !Double precision :: vx, vy, vz
        write(fp, "(*(G0, ' '))") CELLS(id)%vx, CELLS(id)%vy, CELLS(id)%vz
        !Double precision :: fx, fy, fz
        write(fp, "(*(G0, ' '))") CELLS(id)%fx, CELLS(id)%fy, CELLS(id)%fz
        !Double precision :: cr, cg, cb
        write(fp, "(*(G0, ' '))") CELLS(id)%cr, CELLS(id)%cg, CELLS(id)%cb
        !Double precision :: ar, ag, ab
        write(fp, "(*(G0, ' '))") CELLS(id)%ar, CELLS(id)%ag, CELLS(id)%ab
        !Double precision :: IOLr  !Intensity Of Light r
        write(fp, "(*(G0, ' '))") CELLS(id)%IOLr
        !Double precision :: IOLg  !Intensity Of Light g
        write(fp, "(*(G0, ' '))") CELLS(id)%IOLg
        !Double precision :: IOLb  !Intensity Of Light b
        write(fp, "(*(G0, ' '))") CELLS(id)%IOLb
        !Double precision :: m
        write(fp, "(*(G0, ' '))") CELLS(id)%m
        !Double precision :: r
        write(fp, "(*(G0, ' '))") CELLS(id)%r
        !Double precision :: E
        write(fp, "(*(G0, ' '))") CELLS(id)%E
        !Double precision :: alpha
        write(fp, "(*(G0, ' '))") CELLS(id)%alpha

        !Integer :: ID
        write(fp, "(*(G0, ' '))") CELLS(id)%ID

        !Integer :: age
        write(fp, "(*(G0, ' '))") CELLS(id)%age

        !Double precision :: in_data(NOIU)
        write(fp, "(*(G0, ' '))") CELLS(id)%in_data(:)
        !Double precision :: out_data(NOOU)
        write(fp, "(*(G0, ' '))") CELLS(id)%out_data(:)
        !Double precision :: w1(NOHU,(NOIU+1))
        write(fp, "(*(G0, ' '))") CELLS(id)%w1(:,:)
        !Double precision :: w2(NOOU,(NOHU+1))
        write(fp, "(*(G0, ' '))") CELLS(id)%w2(:,:)

        !Double precision :: KOCC(NOCC)    !K Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%KOCC(:)
        !Double precision :: LOCC(NOCC)    !L Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%LOCC(:)
        !Double precision :: SOCC(NOCC)    !S Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%SOCC(:)

        !Integer :: CCF(NOCC)      !Connected Cell Flag
        write(fp, "(*(G0, ' '))") CELLS(id)%CCF(:)
        !Integer :: IOCC(NOCC)     !Id Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%IOCC(:)
        !Integer :: UIOCC(NOCC)    !Unit Id Of Connected Cell
        write(fp, "(*(G0, ' '))") CELLS(id)%UIOCC(:)

        !Integer :: HIT_BLOCK_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%HIT_BLOCK_AF
        !Integer :: HIT_CELL_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%HIT_CELL_AF
        !Integer :: SPRING_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%SPRING_AF
        !Integer :: MECHANICS_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%MECHANICS_AF

        !Integer :: EAT_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%EAT_AF
        !Integer :: FUSION_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%FUSION_AF
        !Integer :: LIGHT_AF
        write(fp, "(*(G0, ' '))") CELLS(id)%LIGHT_AF

        !Integer :: INFO_TRANS_F
        write(fp, "(*(G0, ' '))") CELLS(id)%INFO_TRANS_F
        !Integer :: NEURAL_NETWORK_F
        write(fp, "(*(G0, ' '))") CELLS(id)%NEURAL_NETWORK_F

        !Integer :: WAIT_FOR_CONNECT_F
        write(fp, "(*(G0, ' '))") CELLS(id)%WAIT_FOR_CONNECT_F
        !Integer :: WAIT_FOR_CONNECT_UI
        write(fp, "(*(G0, ' '))") CELLS(id)%WAIT_FOR_CONNECT_UI
        !Integer :: WAIT_FOR_DISCONNECT_F
        write(fp, "(*(G0, ' '))") CELLS(id)%WAIT_FOR_DISCONNECT_F

        !Integer :: ALONE_F
        write(fp, "(*(G0, ' '))") CELLS(id)%ALONE_F

    end subroutine write_cell_into_one

    subroutine write_expansion_log
      Integer :: fp=102
      Integer :: id, CALC_CLn 

      open(fp, file="./expansion.log", position="append")
      write(fp, *) WORLD_STEP
      Do CALC_CLn=1, CALC_CLL
         id = CALC_CL(CALC_CLn)
         if(id/=SUN_ID .and. CELLS(id)%INFO_TRANS_F==0) then
            write(fp, *) id, CELLS(id)%E, CELLS(id)%IOCC
         end if
      end Do
      close(fp)
      
    end subroutine write_expansion_log
    
    subroutine read_cells_from_one(basefilename)
        Character(len=*) :: basefilename
        Integer :: id, fp = 12
        Integer :: noc_in_file

        open(fp, file=basefilename)

        read(fp,*) noc_in_file
        
        Do id=1, noc_in_file
           call read_cell_from_one(fp, id)
        end Do

        close(fp)

    end subroutine read_cells_from_one

    subroutine read_cell_from_one(fp, id)
        Character(len=(BOOK_MAX_LENGTH+1)) :: book
        Character(len=(BOOKMARKER_LENGTH+1)) :: bookmarker
        Character(len=(BOOKMARKER_ADVANCE_LENGTH+1)) :: bookmarker_advance
        Integer :: fp, id
        type(cell) :: Rcell

        !write(*, *) 'read cell'
        !write(*, *) '        ID=', id

        !Integer :: exist
        !write(*, *) 'read exist'
        read(fp,*) Rcell%exist

        !Character(len=BOOK_MAX_LENGTH) :: book
        !write(*, *) 'read book'
        read(fp,'(A)') book
        Rcell%book(:) = book(2:)
        !If(len_trim(Rcell%book(:))/=0) then
        !   BOOK_FINITE_COUNT = BOOK_FINITE_COUNT + 1
        !end If

        !Character(len=BOOKMARKER_LENGTH) :: bookmarker
        !write(*, *) 'read bookmarker'
        read(fp,'(A)') bookmarker
        Rcell%bookmarker(:) = bookmarker(2:)

        !write(*, *) 'read bookmarker_advance'
        read(fp,'(A)') bookmarker_advance
        Rcell%bookmarker_advance(:) = bookmarker_advance(2:)
        !write(*, *) 'bookmarker_advance', bookmarker_advance
        !write(*, *) 'Rcell%bookmarker_advance', Rcell%bookmarker_advance

        !Double precision :: x, y, z
        !write(*, *) 'read x, y, z'
        read(fp,*) Rcell%x, Rcell%y, Rcell%z

        !Integer :: nx, ny, nz
        !write(*, *) 'read nx, ny, nz'
        read(fp,*) Rcell%nx, Rcell%ny, Rcell%nz

        !Double precision :: vx, vy, vz
        !write(*, *) 'read vx, vy, vz'
        read(fp,*) Rcell%vx, Rcell%vy, Rcell%vz

        !Double precision :: fx, fy, fz
        !write(*, *) 'read fx, fy, fz'
        read(fp,*) Rcell%fx, Rcell%fy, Rcell%fz

        !Double precision :: cr, cg, cb
        !write(*, *) 'read cr, cg, cb'
        read(fp,*) Rcell%cr, Rcell%cg, Rcell%cb

        !Double precision :: ar, ag, ab
        !write(*, *) 'read ar, ag, ab'
        read(fp,*) Rcell%ar, Rcell%ag, Rcell%ab

        !Double precision :: IOLr  !Intensity Of Light r
        !write(*, *) 'read IOLr  !Intensity Of Light r'
        read(fp,*) Rcell%IOLr

        !Double precision :: IOLg  !Intensity Of Light g
        !write(*, *) 'read IOLg  !Intensity Of Light g'
        read(fp,*) Rcell%IOLg

        !Double precision :: IOLb  !Intensity Of Light b
        !write(*, *) 'read IOLb  !Intensity Of Light b'
        read(fp,*) Rcell%IOLb

        !Double precision :: m
        !write(*, *) 'read m'
        read(fp,*) Rcell%m
        If(Rcell%m < MIN_M) then
           Rcell%m = MIN_M
        else If(MAX_M<Rcell%m) then
           Rcell%m = MAX_M
        end If
        !Double precision :: r
        !write(*, *) 'read r'
        read(fp,*) Rcell%r

        !Double precision :: E
        !write(*, *) 'read E'
        read(fp,*) Rcell%E

        !Double precision :: alpha
        !write(*, *) 'read alpha'
        read(fp,*) Rcell%alpha

        !Integer :: ID
        !write(*, *) 'read ID'
        read(fp,*) Rcell%ID

        !write(*,*) Rcell%ID, Rcell%exist, Rcell%E, Rcell%x, Rcell%y, Rcell%z

        !Integer :: age
        !write(*, *) 'read age'
        read(fp,*) Rcell%age
        
        !Double precision :: in_data(NOIU)
        !write(*, *) 'read in_data(NOIU)'
        read(fp,*) Rcell%in_data(:)

        !Double precision :: out_data(NOOU)
        !write(*, *) 'read out_data(NOOU)'
        read(fp,*) Rcell%out_data(:)

        !Double precision :: w1(NOHU,(NOIU+1))
        !write(*, *) 'read w1(NOHU,(NOIU+1))'
        read(fp,*) Rcell%w1(:,:)

        !Double precision :: w2(NOOU,(NOHU+1))
        !write(*, *) 'read w2(NOOU,(NOHU+1))'
        read(fp,*) Rcell%w2(:,:)

        !Double precision :: KOCC(NOCC)    !K Of Connected Cell
        !write(*, *) 'read KOCC(NOCC)    !K Of Connected Cell'
        read(fp,*) Rcell%KOCC(:)

        !Double precision :: LOCC(NOCC)    !L Of Connected Cell
        !write(*, *) 'read LOCC(NOCC)    !L Of Connected Cell'
        read(fp,*) Rcell%LOCC(:)

        !Double precision :: SOCC(NOCC)    !S Of Connected Cell
        !write(*, *) 'read SOCC(NOCC)    !S Of Connected Cell'
        read(fp,*) Rcell%SOCC(:)

        !Integer :: CCF(NOCC)      !Connected Cell Flag
        !write(*, *) 'read CCF(NOCC)      !Connected Cell Flag'
        read(fp,*) Rcell%CCF(:)

        !Integer :: IOCC(NOCC)     !Id Of Connected Cell
        !write(*, *) 'read IOCC(NOCC)     !Id Of Connected Cell'
        read(fp,*) Rcell%IOCC(:)

        !Integer :: UIOCC(NOCC)    !Unit Id Of Connected Cell
        !write(*, *) 'read UIOCC(NOCC)    !Unit Id Of Connected Cell'
        read(fp,*) Rcell%UIOCC(:)

        !Integer :: HIT_BLOCK_AF
        !write(*, *) 'read HIT_BLOCK_AF'
        read(fp,*) Rcell%HIT_BLOCK_AF

        !Integer :: HIT_CELL_AF
        !write(*, *) 'read HIT_CELL_AF'
        read(fp,*) Rcell%HIT_CELL_AF

        !Integer :: SPRING_AF
        !write(*, *) 'read SPRING_AF'
        read(fp,*) Rcell%SPRING_AF

        !Integer :: MECHANICS_AF
        !write(*, *) 'read MECHANICS_AF'
        read(fp,*) Rcell%MECHANICS_AF

        !Integer :: EAT_AF
        !write(*, *) 'read EAT_AF'
        read(fp,*) Rcell%EAT_AF

        !Integer :: FUSION_AF
        !write(*, *) 'read FUSION_AF'
        read(fp,*) Rcell%FUSION_AF

        !Integer :: LIGHT_AF
        !write(*, *) 'read LIGHT_AF'
        read(fp,*) Rcell%LIGHT_AF

        !Integer :: INFO_TRANS_F
        !write(*, *) 'read INFO_TRANS_F'
        read(fp,*) Rcell%INFO_TRANS_F

        !Integer :: NEURAL_NETWORK_F
        !write(*, *) 'read NEURAL_NETWORK_F'
        read(fp,*) Rcell%NEURAL_NETWORK_F

        !Integer :: WAIT_FOR_CONNECT_F
        !write(*, *) 'read WAIT_FOR_CONNECT_F'
        read(fp,*) Rcell%WAIT_FOR_CONNECT_F

        !Integer :: WAIT_FOR_CONNECT_UI
        !write(*, *) 'read WAIT_FOR_CONNECT_UI'
        read(fp,*) Rcell%WAIT_FOR_CONNECT_UI

        !Integer :: WAIT_FOR_DISCONNECT_F
        !write(*, *) 'read WAIT_FOR_DISCONNECT_F'
        read(fp,*) Rcell%WAIT_FOR_DISCONNECT_F

        !Integer :: ALONE_F
        !write(*, *) 'read ALONE_F'
        read(fp,*) Rcell%ALONE_F

        If(id==0) then
            CELLS(Rcell%id) = Rcell
        else
            CELLS(id) = Rcell
        end If
    end subroutine read_cell_from_one

    subroutine create_tetrahedra_cell(id, cx, cy, cz, E)
        Character(len=BOOK_MAX_LENGTH) :: book
        Character(len=BOOKMARKER_LENGTH) :: bookmarker
        Character(len=BOOKMARKER_ADVANCE_LENGTH) :: bookmarker_advance
        Integer :: i, j, id, bp
        Double precision :: cx, cy, cz
        Double precision :: E

        bookmarker = 'aa'
        bookmarker_advance = 'A'
        
        book(1:2) = 'SS'
        bp = 3
        
        book(bp:(bp+1)) = 'aa'
        bp = bp + 2
        book(bp:bp) = 'A'
        bp = bp + 1

        !ar
        book(bp:(bp+BDAD-1)) = 'A-'
        bp = bp + BDAD

        !ag
        book(bp:(bp+BDAD-1)) = '--'
        bp = bp + BDAD

        !ab
        book(bp:(bp+BDAD-1)) = '--'
        bp = bp + BDAD

        !Ir
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !Ig
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !Ib
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !m
        book(bp:(bp+BDMD-1)) = 'H1'
        bp = bp + BDMD

        !r
        book(bp:(bp+BDRD-1)) = '--'
        bp = bp + BDRD

        Do i=1, NOHU
            Do j=1, (NOIU+1)
                book(bp:(bp+BDWD-1)) = '--'
                bp = bp + BDWD
            end Do
        end Do

        Do i=1, NOOU
            Do j=1, (NOHU+1)
                If(j==1) then
                    If(i==ODUIO_TRANSF) then
                        book(bp:(bp+BDWD-1)) = '--'
                    else
                        book(bp:(bp+BDWD-1)) = 'f-'
                    end If
                else
                    book(bp:(bp+BDWD-1)) = 'gA'
                end If
                bp = bp + BDWD
            end Do
        end Do

        !book start point
        book(bp:(bp+2)) = '0+0'
        bp = bp + 3
        !book end point
        book(bp:(bp+2)) = '1+1'
        bp = bp + 3
        !bookmark for new cell
        book(bp:(bp+2)) = '0+0'
        bp = bp + 3
        !bookmark_advance for new cell
        book(bp:bp) = 'A'
        bp = bp + 1
        !new bookmark
        book(bp:(bp+2)) = 'b+b'
        bp = bp + 3
        !new bookmark_advance
        book(bp:bp) = 'A'
        bp = bp + 1
        
        !-----------------------------------------------
        book(bp:(bp+1)) = 'bb'
        bp = bp + 2
        book(bp:bp) = 'A'
        bp = bp + 1

        !ar
        book(bp:(bp+BDAD-1)) = '--'
        bp = bp + BDAD

        !ag
        book(bp:(bp+BDAD-1)) = 'A-'
        bp = bp + BDAD

        !ab
        book(bp:(bp+BDAD-1)) = '--'
        bp = bp + BDAD

        !Ir
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !Ig
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !Ib
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !m
        book(bp:(bp+BDMD-1)) = 'H1'
        bp = bp + BDMD

        !r
        book(bp:(bp+BDRD-1)) = '--'
        bp = bp + BDRD

        Do i=1, NOHU
            Do j=1, (NOIU+1)
                book(bp:(bp+BDWD-1)) = '--'
                bp = bp + BDWD
            end Do
        end Do

        Do i=1, NOOU
            Do j=1, (NOHU+1)
                If(j==1) then
                    If(i==ODUIO_TRANSF) then
                        book(bp:(bp+BDWD-1)) = '--'
                    else
                        book(bp:(bp+BDWD-1)) = 'f-'
                    end If
                else
                    book(bp:(bp+BDWD-1)) = 'gA'
                end If
                bp = bp + BDWD
            end Do
        end Do

        !book start point
        book(bp:(bp+2)) = '0+0'
        bp = bp + 3
        !book end point
        book(bp:(bp+2)) = '1+1'
        bp = bp + 3
        !bookmark for new cell
        book(bp:(bp+2)) = '0+0'
        bp = bp + 3
        !bookmark_advance for new cell
        book(bp:bp) = 'A'
        bp = bp + 1
        !new bookmark
        book(bp:(bp+2)) = 'c+c'
        bp = bp + 3
        !new bookmark_advance
        book(bp:bp) = 'A'
        bp = bp + 1

        !-----------------------------------------------
        book(bp:(bp+1)) = 'cc'
        bp = bp + 2
        book(bp:bp) = 'A'
        bp = bp + 1

        !ar
        book(bp:(bp+BDAD-1)) = '--'
        bp = bp + BDAD

        !ag
        book(bp:(bp+BDAD-1)) = '--'
        bp = bp + BDAD

        !ab
        book(bp:(bp+BDAD-1)) = 'A-'
        bp = bp + BDAD

        !Ir
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !Ig
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !Ib
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !m
        book(bp:(bp+BDMD-1)) = 'H1'
        bp = bp + BDMD

        !r
        book(bp:(bp+BDRD-1)) = '--'
        bp = bp + BDRD

        Do i=1, NOHU
            Do j=1, (NOIU+1)
                book(bp:(bp+BDWD-1)) = '--'
                bp = bp + BDWD
            end Do
        end Do

        Do i=1, NOOU
            Do j=1, (NOHU+1)
                If(j==1) then
                    If(i==ODUIO_TRANSF) then
                        book(bp:(bp+BDWD-1)) = '--'
                    else
                        book(bp:(bp+BDWD-1)) = 'f-'
                    end If
                else
                    book(bp:(bp+BDWD-1)) = 'gA'
                end If
                bp = bp + BDWD
            end Do
        end Do

        !book start point
        book(bp:(bp+2)) = '0+0'
        bp = bp + 3
        !book end point
        book(bp:(bp+2)) = '1+1'
        bp = bp + 3
        !bookmark for new cell
        book(bp:(bp+2)) = '0+0'
        bp = bp + 3
        !bookmark_advance for new cell
        book(bp:bp) = 'A'
        bp = bp + 1
        !new bookmark
        book(bp:(bp+2)) = 'd+d'
        bp = bp + 3
        !new bookmark_advance
        book(bp:bp) = 'A'
        bp = bp + 1

        !-----------------------------------------------
        book(bp:(bp+1)) = 'dd'
        bp = bp + 2
        book(bp:bp) = 'A'
        bp = bp + 1

        !ar
        book(bp:(bp+BDAD-1)) = '5-'
        bp = bp + BDAD

        !ag
        !book(bp:(bp+BDAD-1)) = '--'
        book(bp:(bp+BDAD-1)) = '5-'
        bp = bp + BDAD

        !ab
        !book(bp:(bp+BDAD-1)) = '--'
        book(bp:(bp+BDAD-1)) = '5-'
        bp = bp + BDAD

        !Ir
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !Ig
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !Ib
        book(bp:(bp+BDID-1)) = 'BA'
        bp = bp + BDID

        !m
        book(bp:(bp+BDMD-1)) = 'H1'
        bp = bp + BDMD

        !r
        book(bp:(bp+BDRD-1)) = '--'
        bp = bp + BDRD

        Do i=1, NOHU
            Do j=1, (NOIU+1)
                book(bp:(bp+BDWD-1)) = '--'
                bp = bp + BDWD
            end Do
        end Do

        Do i=1, NOOU
            Do j=1, (NOHU+1)
                If(j==1) then
                    If(i==ODUIO_TRANSF) then
                        book(bp:(bp+BDWD-1)) = '--'
                    else
                        book(bp:(bp+BDWD-1)) = 'f-'
                    end If
                else
                    book(bp:(bp+BDWD-1)) = 'gA'
                end If
                bp = bp + BDWD
            end Do
        end Do

        !book start point
        book(bp:(bp+2)) = 'S+S'
        bp = bp + 3
        !book end point
        book(bp:(bp+2)) = 'E+E'
        bp = bp + 3
        !bookmark for new cell
        book(bp:(bp+2)) = 'e+e'
        bp = bp + 3
        !bookmark_advance for new cell
        book(bp:bp) = 'E'
        bp = bp + 1
        !new bookmark
        book(bp:(bp+2)) = 'e+e'
        bp = bp + 3
        !new bookmark_advance
        book(bp:bp) = 'A'
        bp = bp + 1


        !-----------------------------------------------
        book(bp:(bp+1)) = 'ee'
        bp = bp + 2
        book(bp:bp) = 'g'
        bp = bp + 1
        book(bp:(bp+2)) = 'd+d'
        bp = bp + 3
        book(bp:bp) = 'A'
        bp = bp + 1
        book(bp:(bp+2)) = 'a+a'
        bp = bp + 3
        book(bp:bp) = 'A'
        bp = bp + 1
        
        !-----------------------------------------------
        book(bp:(bp+1)) = '00'
        bp = bp + 2
        book(bp:bp) = 'Q'
        bp = bp + 1
        book(bp:(bp+2)) = '0+0'
        bp = bp + 3
        book(bp:bp) = 'E'
        bp = bp + 1
        book(bp:(bp+2)) = '2+2'
        bp = bp + 3
        book(bp:(bp+1)) = '11'
        bp = bp + 2
        
        book(bp:(bp+1)) = 'EE'
        bp = bp + 2

        CELLS(id)%exist = 1
        CELLS(id)%book(:) = book(:)
        CELLS(id)%bookmarker = bookmarker
        CELLS(id)%bookmarker_advance = bookmarker_advance

        CELLS(id)%x = cx
        CELLS(id)%y = cy
        CELLS(id)%z = cz

        CELLS(id)%vx = 0.0d0
        CELLS(id)%vy = 0.0d0
        CELLS(id)%vz = 0.0d0

        CELLS(id)%fx = 0.0d0
        CELLS(id)%fy = 0.0d0
        CELLS(id)%fz = 0.0d0

        CELLS(id)%ar = 0.371d0/0.4095d0
        CELLS(id)%ag = 0.371d0/0.4095d0
        CELLS(id)%ab = 0.371d0/0.4095d0

        CELLS(id)%cr = 1.0d0 - CELLS(id)%ar
        CELLS(id)%cg = 1.0d0 - CELLS(id)%ag
        CELLS(id)%cb = 1.0d0 - CELLS(id)%ab

        CELLS(id)%IOLr = 0.01d0
        CELLS(id)%IOLg = 0.01d0
        CELLS(id)%IOLb = 0.01d0

        CELLS(id)%m = 0.501d0
        CELLS(id)%r = 0.2d0

        CELLS(id)%E = E

        CELLS(id)%alpha = 0.0d0

        CELLS(id)%ID  = id
        CELLS(id)%age = 0

        CELLS(id)%in_data(:) = 0.0d0
        CELLS(id)%out_data(:) = 0.0d0

        Do i=1, NOHU
            Do j=1, (NOIU+1)
                CELLS(id)%w1(i,j) = 0.0d0
            end Do
        end Do

        Do i=1, NOOU
            Do j=1, (NOHU+1)
                If(j==1) then
                    If(i==ODUIO_TRANSF) then
                        CELLS(id)%w2(i,j) = 1.0d0
                    else
                        CELLS(id)%w2(i,j) = -0.1d0
                    end If
                else
                    CELLS(id)%w2(i,j) = 0.0d0
                end If
            end Do
        end Do

        CELLS(id)%KOCC(:) = 0.0d0
        CELLS(id)%LOCC(:) = 0.0d0
        CELLS(id)%SOCC(:) = 0.0d0

        CELLS(id)%CCF(:) = 0
        CELLS(id)%IOCC(:) = 0
        CELLS(id)%UIOCC(:) = 0

        CELLS(id)%HIT_BLOCK_AF = 1
        CELLS(id)%HIT_CELL_AF = 1
        CELLS(id)%SPRING_AF = 1
        CELLS(id)%MECHANICS_AF = 1
        CELLS(id)%EAT_AF = 0
        CELLS(id)%FUSION_AF = 0
        CELLS(id)%LIGHT_AF = 0
        CELLS(id)%INFO_TRANS_F = 0
        CELLS(id)%NEURAL_NETWORK_F = 1
        CELLS(id)%WAIT_FOR_CONNECT_F = 0
        CELLS(id)%WAIT_FOR_CONNECT_UI = 0
        CELLS(id)%WAIT_FOR_DISCONNECT_F = 0
        CELLS(id)%ALONE_F = 1

    end subroutine create_tetrahedra_cell   

    subroutine turn_off_the_sun()  bind(c)
        CELLS(SUN_ID)%LIGHT_AF = 0
    end subroutine turn_off_the_sun

    subroutine turn_on_the_sun()  bind(c)
        CELLS(SUN_ID)%LIGHT_AF = 1
    end subroutine turn_on_the_sun

    subroutine switch_daynight()  bind(c)
        NIGHT_FLAG = 1 - NIGHT_FLAG
        CELLS(SUN_ID)%LIGHT_AF = 1 - NIGHT_FLAG
    end subroutine switch_daynight
    
    subroutine step(n) bind(c)
        Integer(c_int) n 
        Integer :: i, j, n_out, n_in 
        Double precision :: sun_theta
        Double precision :: gx, gy, gz
        Double precision :: alpha

        n_in  = OUT_INTERVAL_STEP
        n_out = n/n_in

        Do i=1, n_out
            
            if((CALC_CLL-NUMBER_OF_INFO_TRANS)<ABS_KEEP_NUM) then
               EVERY_STEP_COST_E = 0.0d0
            else
               EVERY_STEP_COST_E = ORG_EVERY_STEP_COST_E
            end if            

            NUMBER_OF_CELLS_WOIT = CALC_CLL-NUMBER_OF_INFO_TRANS
            TOT_ENERGY_FOR_C     = TOT_ENERGY
            gx = CENTER_OF_GRAV_X/TOT_M
            gy = CENTER_OF_GRAV_Y/TOT_M
            gz = CENTER_OF_GRAV_Z/TOT_M
            sun_theta = 2.0d0*PI*(dble(WORLD_STEP)/SUN_CYCLE)
            write(*, "(*(G0, ' '))") '#####################################################'
            write(*, "(*(G0, ' '))") 'WORLD_STEP             =', WORLD_STEP
            write(*, "(*(G0, ' '))") 'CALC_CLL               =', CALC_CLL
            write(*, "(*(G0, ' '))") 'NOT_CALC_CLL           =', NOT_CALC_CLL
            write(*, "(*(G0, ' '))") 'NUMBER_OF_INFO_TRANS   =', NUMBER_OF_INFO_TRANS
            write(*, "(*(G0, ' '))") 'CALC_CLL-INFO_TRANS    =', NUMBER_OF_CELLS_WOIT
            write(*, "(*(G0, ' '))") 'CALC_CLL+NOT_CALC_CLL  =', CALC_CLL+NOT_CALC_CLL
            write(*, "(*(G0, ' '))") 'CONN_CLL               =', CONN_CLL
            write(*, "(*(G0, ' '))") 'EAT_COUNT*             =', EAT_COUNT
            write(*, "(*(G0, ' '))") 'EAT_INFO*              =', EAT_INFO
            write(*, "(*(G0, ' '))") 'EATEN_INFO*            =', EATEN_INFO
            write(*, "(*(G0, ' '))") 'FUSION_COUNT*          =', FUSION_COUNT
            write(*, "(*(G0, ' '))") 'EXPANSION_COUNT*       =', EXPANSION_COUNT
            write(*, "(*(G0, ' '))") 'CONNECT_COUNT*         =', CONNECT_COUNT
            write(*, "(*(G0, ' '))") 'DISCONNECT_COUNT*      =', DISCONNECT_COUNT
            write(*, "(*(G0, ' '))") 'TURN_BOOKMARKER_COUNT* =', TURN_BOOKMARKER_COUNT
            write(*, "(*(G0, ' '))") 'MUTATION_COUNT*        =', MUTATION_COUNT
            write(*, "(*(G0, ' '))") 'DEATHS_COUNT*          =', DEATHS_COUNT
            write(*, "(*(G0, ' '))") 'CELLS(SUN_ID)%LIGHT_AF =', SUN_ID, CELLS(SUN_ID)%LIGHT_AF, NIGHT_FLAG 
            write(*, "(*(G0, ' '))") 'sun_theta              =', sun_theta
            write(*, "(*(G0, ' '))") 'TOT_ENERGY*            =', TOT_ENERGY
            write(*, "(*(G0, ' '))") 'AVE_ENERGY*            =', TOT_ENERGY/dble(CALC_CLL-NUMBER_OF_INFO_TRANS)
            write(*, "(*(G0, ' '))") 'EVERY_STEP_COST_A      =', EVERY_STEP_COST_A
            write(*, "(*(G0, ' '))") 'CHANGE_L_COUNT*        =', CHANGE_L_COUNT
            write(*, "(*(G0, ' '))") 'ENERGY_TRANSIT_COUNT*  =', ENERGY_TRANSIT_COUNT
            write(*, "(*(G0, ' '))") 'CENTER_OF_GRAV         =', gx, gy, gz
            write(*, "(*(G0, ' '))") 'AGE INFO FOR CELL      =', AGE_AVE, AGE_VAR, AGE_MIN, AGE_MAX
            write(*, "(*(G0, ' '))") 'AGE INFO FOR IT        =', AGE_IT_AVE, AGE_IT_VAR, AGE_IT_MIN, AGE_IT_MAX
            write(*, "(*(G0, ' '))") 'TOT_M*                 =', TOT_M
            write(*, "(*(G0, ' '))") 'ALONE_COUNT*           =', ALONE_COUNT
            write(*, "(*(G0, ' '))") '#####################################################'
            EAT_COUNT = 0
            EAT_INFO(:) = 0
            EATEN_INFO(:) = 0
            FUSION_COUNT = 0
            EXPANSION_COUNT = 0
            CONNECT_COUNT = 0
            DISCONNECT_COUNT = 0
            TURN_BOOKMARKER_COUNT = 0
            MUTATION_COUNT = 0
            DEATHS_COUNT = 0
            CENTER_OF_GRAV_X = 0.0d0
            CENTER_OF_GRAV_Y = 0.0d0
            CENTER_OF_GRAV_Z = 0.0d0
            TOT_M = 0.0d0
            TOT_ENERGY = 0.0d0
            CHANGE_L_COUNT = 0
            ENERGY_TRANSIT_COUNT = 0
            ALONE_COUNT = 0

            If((CALC_CLL+NOT_CALC_CLL)<NUMBER_OF_CELL) then
                write(*,*) '(CALC_CLL+NOT_CALC_CLL)<NUMBER_OF_CELL !!!!'
            end If

            If(int(0.96d0*dble(NUMBER_OF_CELL)) < CALC_CLL .and. 1000 < WORLD_STEP) then
               write(*, *) 'WARNING!!! CALC_CLL will reach maximum'
               write(*, *) 'STOP THIS PROGRAM'
               call emergency_stop()
            end If

            Do j=1, n_in

               if(LOG_FLAG) then
                  call write_expansion_log()
               end if
               
               if(SUM_LIMIT_NUM<(CALC_CLL-NUMBER_OF_INFO_TRANS)) then
                  CELLS(SUN_ID)%LIGHT_AF = 0
               else
                  if(NIGHT_FLAG==0) then
                     CELLS(SUN_ID)%LIGHT_AF = 1
                  end if
               end if

               if(EVERY_STEP_COST_UPDATE_FLAG) then
                  alpha = dble(TARGET_CELLS_NUM - (CALC_CLL-NUMBER_OF_INFO_TRANS))
                  alpha = EVERY_STEP_COST_D*alpha
                  if(alpha<0.0d0) then
                     if(1.0d0<EVERY_STEP_COST_A) then
                        EVERY_STEP_COST_A = EVERY_STEP_COST_A + alpha
                     end if
                     if(EVERY_STEP_COST_A<1.0d0) then
                        EVERY_STEP_COST_A = 1.0d0
                     end if
                  else
                     EVERY_STEP_COST_A = EVERY_STEP_COST_A + alpha
                  end if
               end if
               
               call sub_step()
               !call sub_step_with_timer()
               
            end Do

        end Do

    end subroutine step

    subroutine sub_step()

        !Double precision :: r
        !call random_number(r)
        !write(*, *) "! r=", r
      
        WORLD_STEP = WORLD_STEP + 1
        
        !call timer('start', time, 'cpu')
        call calculation_cell_neural_network()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:calculation_cell_neural_network', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call action_cell()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:action_cell', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call light_calculation()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:light_calculation', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call cell_hit_block()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:cell_hit_block', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call cell_hit_cell2()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:cell_hit_cell2', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call cell_spring_cell()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:cell_spring_cell', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call cell_mechanics()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:cell_mechanics', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call update_cell()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:update_cell', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call update_calculation_cell_list()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:update_calculation_cell_list', time, time/dble(CALC_CLL)

        !call timer('start', time, 'cpu')
        call update_CONN_CL()
        !call timer('end', time, 'cpu')
        !write(*,*) 'Time:update_CONN_CL', time, time/dble(CALC_CLL)

        !-!call cpu_timer('end', ts, te, td)
        !-!write(*,*) 'Time:WHOLETIME', td, td/dble(CALC_CLL)

    end subroutine sub_step

    subroutine sub_step_with_timer()
        Double precision :: time
        WORLD_STEP = WORLD_STEP + 1
        
        call timer('start', time, 'cpu')
        call calculation_cell_neural_network()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:calculation_cell_neural_network', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        call action_cell()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:action_cell', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        call light_calculation()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:light_calculation', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        call cell_hit_block()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:cell_hit_block', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        !call cell_hit_cell()
        call cell_hit_cell2()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:cell_hit_cell2', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        call cell_spring_cell()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:cell_spring_cell', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        call cell_mechanics()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:cell_mechanics', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        call update_cell()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:update_cell', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        call update_calculation_cell_list()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:update_calculation_cell_list', time, time/dble(CALC_CLL)

        call timer('start', time, 'cpu')
        call update_CONN_CL()
        call timer('end', time, 'cpu')
        write(*,*) 'Time:update_CONN_CL', time, time/dble(CALC_CLL)

        !-!call cpu_timer('end', ts, te, td)
        !-!write(*,*) 'Time:WHOLETIME', td, td/dble(CALC_CLL)

    end subroutine sub_step_with_timer

    subroutine emergency_stop() 
        write(*, *) 'EMERGENCY STOP IS CALLED!!!!'
        write(*, *) 'STOP THIS PROGRAM'
        call write_cells('./emergency_stop_cells/data')
        stop        
    end subroutine emergency_stop

    subroutine temporary_export(n) bind(c)
        Character(len=128) :: filename
        Integer(c_int) :: n
        !write(*, *) 'TEMPORARY EXPORT IS CALLED!!!!'
        !call write_cells('./temporary/data')
        !call write_cells_into_one('./tmp_cells_file')
        write (filename, '("./cells_file", i0)') n
        call write_cells_into_one(trim(adjustl(filename)))
    end subroutine temporary_export
    
    subroutine init_calculation_cell_list_first()
        Integer :: id 
        CALC_CLL = 0
        CALC_CL(:) = 0
        NOT_CALC_CLL = 0
        NOT_CALC_CL(:) = 0
        CELLS_BOTTOM = 1.0d+17
        Do id=1, NUMBER_OF_CELL
            If(CELLS(id)%exist==1) then
                if(CELLS(id)%INFO_TRANS_F/=1 .and. CELLS(id)%y<CELLS_BOTTOM) then
                   CELLS_BOTTOM = CELLS(id)%y
                end if
                
                CALC_CLL = CALC_CLL + 1
                CALC_CL(CALC_CLL) = id
            else
                NOT_CALC_CLL = NOT_CALC_CLL + 1
                NOT_CALC_CL(NOT_CALC_CLL) = id
            end If
        end Do
        BACK_CALC_CLL = CALC_CLL
        BACK_CALC_CL(:) = CALC_CL(:)
        NEW_NOT_CALC_CLL = NOT_CALC_CLL
    end subroutine init_calculation_cell_list_first

    subroutine init_calculation_cell_list_second()
        Integer :: id, BACK_CALC_CLn
        Integer :: i, j, k
        FIELD(:,:,:)%NOC = 0
        NUMBER_OF_INFO_TRANS = 0
        BACK_CALC_CLn = 1
        Do
            If(BACK_CALC_CLL<BACK_CALC_CLn) then
                exit
            end If
            id = BACK_CALC_CL(BACK_CALC_CLn)
            if(id/=SUN_ID .and. FIELD_ADJUST_FLAG==1) then
               CELLS(id)%y = CELLS(id)%y - (CELLS_BOTTOM - FIELD_TOP)
            end if

            i = nint(CELLS(id)%x + dble(FIELD_SIZE_X/2 - FIELD_CENTER_X)) + 1
            j = nint(CELLS(id)%y + dble(FIELD_SIZE_Y/2 - FIELD_CENTER_Y)) + 1
            k = nint(CELLS(id)%z + dble(FIELD_SIZE_Z/2 - FIELD_CENTER_Z)) + 1

            CELLS(id)%nx = i
            CELLS(id)%ny = j
            CELLS(id)%nz = k

            If(i<2 .or. (FIELD_SIZE_X-1)<i) then
                call free_cell(id)
                call delete_BACK_CALC_CL(id)
                call add_NEW_NOT_CALC_CL(id)
                cycle
            end If

            If(j<DEPTH_LIMIT .or. (FIELD_SIZE_Y-1)<j) then
                call free_cell(id)
                call delete_BACK_CALC_CL(id)
                call add_NEW_NOT_CALC_CL(id)
                cycle
            end If

            If(k<2 .or. (FIELD_SIZE_Z-1)<k) then
                call free_cell(id)
                call delete_BACK_CALC_CL(id)
                call add_NEW_NOT_CALC_CL(id)
                cycle
            end If

            If(0.0d0<HEIGHT_LIMIT .and. HEIGHT_LIMIT<CELLS(id)%y) then
               if(id/=SUN_ID .and. CELLS(id)%INFO_TRANS_F/=1) then
                  call free_cell(id)
                  call delete_BACK_CALC_CL(id)
                  call add_NEW_NOT_CALC_CL(id)
                  cycle
               end if
            end If

            If(CELLS(id)%INFO_TRANS_F==1) then
                NUMBER_OF_INFO_TRANS = NUMBER_OF_INFO_TRANS + 1
            end If

            If(FIELD(i,j,k)%NOC<CELL_CAPACITY_OF_BLOCK) then
               FIELD(i,j,k)%NOC = FIELD(i,j,k)%NOC + 1
               FIELD(i,j,k)%IOC(FIELD(i,j,k)%NOC) = id
            else
               write(*, *) 'Number of the cell over the limit!!', i, j, k
            end If

            BACK_CALC_CLn = BACK_CALC_CLn + 1
        end Do
        CALC_CLL = BACK_CALC_CLL
        CALC_CL(:) = BACK_CALC_CL(:)
        NOT_CALC_CLL = NEW_NOT_CALC_CLL
    end subroutine init_calculation_cell_list_second  
    
    subroutine update_calculation_cell_list()
        Integer :: id, BACK_CALC_CLn
        Integer :: i, j, k
        FIELD(:,:,:)%NOC = 0
        NUMBER_OF_INFO_TRANS = 0
        BACK_CALC_CLn = 1

        Do
            If(BACK_CALC_CLL<BACK_CALC_CLn) then
                exit
            end If
            id = BACK_CALC_CL(BACK_CALC_CLn)

            i = nint(CELLS(id)%x + dble(FIELD_SIZE_X/2 - FIELD_CENTER_X)) + 1
            j = nint(CELLS(id)%y + dble(FIELD_SIZE_Y/2 - FIELD_CENTER_Y)) + 1
            k = nint(CELLS(id)%z + dble(FIELD_SIZE_Z/2 - FIELD_CENTER_Z)) + 1

            CELLS(id)%nx = i
            CELLS(id)%ny = j
            CELLS(id)%nz = k

            If(i<2 .or. (FIELD_SIZE_X-1)<i) then
                call free_cell(id)
                call delete_BACK_CALC_CL(id)
                call add_NEW_NOT_CALC_CL(id)
                cycle
            end If

            If(j<DEPTH_LIMIT .or. (FIELD_SIZE_Y-1)<j) then
                call free_cell(id)
                call delete_BACK_CALC_CL(id)
                call add_NEW_NOT_CALC_CL(id)
                cycle
            end If

            If(k<2 .or. (FIELD_SIZE_Z-1)<k) then
                call free_cell(id)
                call delete_BACK_CALC_CL(id)
                call add_NEW_NOT_CALC_CL(id)
                cycle
            end If

            If(0.0d0<HEIGHT_LIMIT .and. HEIGHT_LIMIT<CELLS(id)%y) then
               if(id/=SUN_ID .and. CELLS(id)%INFO_TRANS_F/=1) then
                  call free_cell(id)
                  call delete_BACK_CALC_CL(id)
                  call add_NEW_NOT_CALC_CL(id)
                  cycle
               end if
            end If
            
            If(CELLS(id)%INFO_TRANS_F==1) then
                NUMBER_OF_INFO_TRANS = NUMBER_OF_INFO_TRANS + 1
            end If

            If(FIELD(i,j,k)%NOC<CELL_CAPACITY_OF_BLOCK) then
               FIELD(i,j,k)%NOC = FIELD(i,j,k)%NOC + 1
               FIELD(i,j,k)%IOC(FIELD(i,j,k)%NOC) = id
            else
               write(*, *) 'Number of the cell over the limit!!', i, j, k
            end If

            BACK_CALC_CLn = BACK_CALC_CLn + 1
        end Do
        CALC_CLL = BACK_CALC_CLL
        CALC_CL(:) = BACK_CALC_CL(:)
        NOT_CALC_CLL = NEW_NOT_CALC_CLL
    end subroutine update_calculation_cell_list

    subroutine init_CONN_CL()
        Integer :: CALC_CLn, k1, k2, count, id1, id2
        Integer, allocatable :: check(:, :)

        CONN_CL(:, :, :) = 0

        allocate(check(NUMBER_OF_CELL, NUMBER_OF_CELL))
        check(:,:) = 0

        count = 1

        Do CALC_CLn=1, CALC_CLL
            id1 = CALC_CL(CALC_CLn)
            Do k1=1, NOCC
                If(CELLS(id1)%CCF(k1)==1) then
                    id2 = CELLS(id1)%IOCC(k1)
                    Do k2=1, NOCC
                        If(CELLS(id2)%IOCC(k2)==id1) then
                            check(id1,id2) = 1
                            If(check(id2,id1)==0) then
                                CONN_CL(count, 1, 1) = id1
                                CONN_CL(count, 1, 2) = k1
                                CONN_CL(count, 2, 1) = id2
                                CONN_CL(count, 2, 2) = k2
                                CELLS(id1)%ALONE_F = 0
                                CELLS(id2)%ALONE_F = 0
                                count = count + 1
                            end If
                            exit
                        end If
                    end Do
                    If(k2==(NOCC+1)) then 
                        write(*,*) 'error from make_CONN_CL', id1, id2, k1, k2
                        CELLS(id1)%IOCC(k1) = 0
                        CELLS(id1)%CCF(k1) = 0
                        CELLS(id1)%UIOCC(k1) = 0
                    end If
                end If
            end Do
        end Do

        CONN_CLL = count - 1

        BACK_CONN_CLL = CONN_CLL
        BACK_CONN_CL(:,:,:) = CONN_CL(:,:,:)

    end subroutine init_CONN_CL

    subroutine update_CONN_CL()
        Integer :: BACK_CONN_CLn
        Do BACK_CONN_CLn=1, BACK_CONN_CLL
            CELLS(BACK_CONN_CL(BACK_CONN_CLn,1,1))%ALONE_F = 0
            CELLS(BACK_CONN_CL(BACK_CONN_CLn,2,1))%ALONE_F = 0
        end Do
        CONN_CLL = BACK_CONN_CLL
        CONN_CL(:,:,:) = BACK_CONN_CL(:,:,:)
    end subroutine update_CONN_CL

    subroutine calculation_cell_neural_network
      Character(len=128) :: filename
      Integer :: fp=12
      Integer :: i, id, tli, CALC_CLn 
      Double precision :: h(NOHU+1)
      Double precision :: in(NOIU+1)
      
      !$omp parallel 
      !$omp do private(id, CALC_CLn)
      Do CALC_CLn=1, CALC_CLL
         id = CALC_CL(CALC_CLn)
         If(id/=SUN_ID .and. CELLS(id)%NEURAL_NETWORK_F==1) then
            call sub_calculation_cell_neural_network(id)
         end If
      end Do
      !$omp end do
      !$omp end parallel

      if(TRACE_FLAG==1) then
         Do tli=1, TRACE_NUM
            id = TRACE_LIST(tli)
            
            in(1) = 1.0d0
            in(2:(NOIU+1)) = CELLS(id)%in_data(:)
            
            h(1) = 1.0d0
            Do i=2, NOHU+1
               h(i) = tanh(sum(in(:)*CELLS(id)%w1((i-1),:)))
            end Do
            
            write(filename, '("trace_", i7.7, "_in.dat")') id
            open(fp, file=trim(filename), status="old", position="append")
            write(fp, *) WORLD_STEP, in(:)
            close(fp)

            write(filename, '("trace_", i7.7, "_h.dat")') id
            open(fp, file=trim(filename), status="old", position="append")
            write(fp, *) WORLD_STEP, h(:)
            close(fp)

            write(filename, '("trace_", i7.7, "_out.dat")') id
            open(fp, file=trim(filename), status="old", position="append")
            write(fp, *) WORLD_STEP, CELLS(id)%out_data(:)
            close(fp)

         end Do
      end if

    end subroutine calculation_cell_neural_network

    subroutine sub_calculation_cell_neural_network(id)
        Integer :: id
        Integer :: i
        Double precision :: h(NOHU+1)
        Double precision :: in(NOIU+1)
        
        in(1) = 1.0d0
        in(2:(NOIU+1)) = CELLS(id)%in_data(:)

        h(1) = 1.0d0
        Do i=2, NOHU+1
            h(i) = tanh(sum(in(:)*CELLS(id)%w1((i-1),:)))
        end Do

        Do i=1, NOOU
            CELLS(id)%out_data(i) = tanh(sum(h(:)*CELLS(id)%w2(i,:)))
        end Do        
    end subroutine sub_calculation_cell_neural_network

    subroutine action_cell()
        Integer :: id, CALC_CLn
        call init_all_cell_in_data()
        Do CALC_CLn=1, CALC_CLL
            id = CALC_CL(CALC_CLn)
            If(CELLS(id)%INFO_TRANS_F==1 .or. id==SUN_ID) then
                cycle
            end If
            call input_cell_in_data(id)
            If(mod(CELLS(id)%age, TRANS_INTERVAL_STEP)==0) then
               call trans_cell_flag_processing(id)
            else If(mod(CELLS(id)%age, RESET_INTERVAL_STEP)==0) then
               CELLS(id)%WAIT_FOR_CONNECT_F    = 0
               CELLS(id)%WAIT_FOR_CONNECT_UI   = 0
               CELLS(id)%WAIT_FOR_DISCONNECT_F = 0
            end If
            If(THRESHOLD_EAT<CELLS(id)%out_data(ODUIO_EAT)) then
               CELLS(id)%EAT_AF = 1
            else
               CELLS(id)%EAT_AF = 0
            end If
            If(THRESHOLD_FUSION<CELLS(id)%out_data(ODUIO_FUSION)) then
               CELLS(id)%FUSION_AF = 1
            else
               CELLS(id)%FUSION_AF = 0
            end If
            If(THRESHOLD_LIGHT<CELLS(id)%out_data(ODUIO_LIGHT)) then
               CELLS(id)%LIGHT_AF = 1
            else
               CELLS(id)%LIGHT_AF = 0
            end If
        end Do
    end subroutine action_cell

    subroutine init_all_cell_in_data()
        Integer :: id, CALC_CLn
        Do CALC_CLn=1, CALC_CLL
            id = CALC_CL(CALC_CLn)
            CELLS(id)%in_data(:) = 0.0d0
        end Do
    end subroutine init_all_cell_in_data

    subroutine input_cell_in_data(id)
        Integer :: id, ccid, ccuid
        Integer :: k
        Double precision :: d, limit_len, dE
        Do k=1, NOCC
            If(CELLS(id)%CCF(k)==1) then
                ccid = CELLS(id)%IOCC(k)
                ccuid = CELLS(id)%UIOCC(k)

                d = CELLS(id)%out_data(k)
                CELLS(ccid)%in_data(ccuid) = CELLS(ccid)%SOCC(ccuid)*d
                CELLS(ccid)%SOCC(ccuid) = CELLS(ccid)%SOCC(ccuid) + RATE_OF_VARI_S*d
                If(CELLS(ccid)%SOCC(ccuid)<0.0d0) then
                    If(CELLS(ccid)%SOCC(ccuid)<(-LIMIT_S)) then
                        CELLS(ccid)%SOCC(ccuid) = -LIMIT_S
                    end If
                else
                    If(LIMIT_S<CELLS(ccid)%SOCC(ccuid)) then
                        CELLS(ccid)%SOCC(ccuid) = LIMIT_S
                    end If
                end If

                d = CELLS(id)%out_data(NOCC + k)
                CELLS(ccid)%in_data(NOCC + ccuid) = d
                If(RANGE_OF_VARI_L_D<abs(d)) then
                    CHANGE_L_COUNT = CHANGE_L_COUNT + 1
                    CELLS(id)%LOCC(k) = (1.0d0+RATE_OF_VARI_L*d)*CELLS(id)%LOCC(k)
                    limit_len = SPRING_LIMIT_C*CELLS(id)%r
                    If(CELLS(id)%LOCC(k)<0.0d0) then
                       CHANGE_L_COUNT = CHANGE_L_COUNT - 1
                       CELLS(id)%LOCC(k) = 0.0d0
                    else If(limit_len<CELLS(id)%LOCC(k)) then
                       CHANGE_L_COUNT = CHANGE_L_COUNT - 1
                       CELLS(id)%LOCC(k) = limit_len
                    end If
                end If

                d = CELLS(id)%out_data(2*NOCC + k)
                CELLS(ccid)%in_data(2*NOCC + ccuid) = d
                If(ENERGY_TRANSIT_FLAG==0) then
                   !Setting 0
                   If(0.0d0<d) then
                      If(ENERGY_TRANSIT_MIN<CELLS(id)%E) then
                         ENERGY_TRANSIT_COUNT = ENERGY_TRANSIT_COUNT + 1
                         CELLS(ccid)%E = CELLS(ccid)%E + ENERGY_TRANSIT_MIN
                         CELLS(id)%E = CELLS(id)%E - ENERGY_TRANSIT_MIN
                      end If
                   end If
                else If(ENERGY_TRANSIT_FLAG==1) then
                   !Setting 1
                   If(0.0d0<d) then
                      If(CELLS(ccid)%E<CELLS(id)%E) then
                         CELLS(ccid)%E = CELLS(ccid)%E + ENERGY_TRANSIT_MIN
                         CELLS(id)%E = CELLS(id)%E - ENERGY_TRANSIT_MIN
                      else
                         CELLS(ccid)%E = CELLS(ccid)%E - ENERGY_TRANSIT_MIN
                         CELLS(id)%E = CELLS(id)%E + ENERGY_TRANSIT_MIN
                      end If
                   end If
                else If(ENERGY_TRANSIT_FLAG==2) then
                   !Setting 2
                   If(0.0d0<d) then
                      If(CELLS(ccid)%E<CELLS(id)%E) then
                         ENERGY_TRANSIT_COUNT = ENERGY_TRANSIT_COUNT + 1
                         dE = ENERGY_TRANSIT_C*(CELLS(id)%E - CELLS(ccid)%E)
                         CELLS(ccid)%E = CELLS(ccid)%E + dE
                         CELLS(id)%E = CELLS(id)%E - dE
                      else
                         ENERGY_TRANSIT_COUNT = ENERGY_TRANSIT_COUNT + 1
                         dE = ENERGY_TRANSIT_C*(CELLS(ccid)%E - CELLS(id)%E)
                         CELLS(ccid)%E = CELLS(ccid)%E - dE
                         CELLS(id)%E = CELLS(id)%E + dE
                      end If
                   end If
                else If(ENERGY_TRANSIT_FLAG==3) then
                   !Setting 3
                   ENERGY_TRANSIT_COUNT = ENERGY_TRANSIT_COUNT + 1
                   dE = d*(CELLS(id)%E - CELLS(ccid)%E)
                   if(0.0d0<dE) then
                      if((CELLS(id)%E - dE)<0.0d0) then
                         dE = CELLS(id)%E
                      end If
                   else
                      if((CELLS(ccid)%E + dE)<0.0d0) then
                         dE = -CELLS(ccid)%E
                      end If
                   end If
                   CELLS(ccid)%E = CELLS(ccid)%E + dE
                   CELLS(id)%E = CELLS(id)%E - dE
                else If(ENERGY_TRANSIT_FLAG==4) then
                   !Setting 4
                   If(0.0d0<d) then
                      ENERGY_TRANSIT_COUNT = ENERGY_TRANSIT_COUNT + 1
                      CELLS(ccid)%E = CELLS(ccid)%E + d*CELLS(id)%E
                      CELLS(id)%E = CELLS(id)%E - d*CELLS(id)%E
                   end If
                else If(ENERGY_TRANSIT_FLAG==5) then
                   !Setting 5
                   If(0.0d0<d) then
                      If(CELLS(ccid)%E<CELLS(id)%E) then
                         ENERGY_TRANSIT_COUNT = ENERGY_TRANSIT_COUNT + 1
                         dE = ENERGY_TRANSIT_C*(CELLS(id)%E - CELLS(ccid)%E)
                         CELLS(ccid)%E = CELLS(ccid)%E + dE
                         CELLS(id)%E = CELLS(id)%E - dE
                      end If
                   end If
                end If

            end If
        end Do
    end subroutine input_cell_in_data

    subroutine trans_cell_flag_processing(id)
        Character(len=1) :: trans_flag
        Character(len=1) :: mutation_char
        Character(len=BOOKMARKER_LENGTH) :: mutation_bookmarker
        Integer :: id, i, n, m, qbook_length, bp, k, act_NBLTE, act_NBLTC, act_NBLTD, act_NBLTT, bpfb
        Double precision :: x
        Double precision :: act_mutation_rate

        call return_mutation_rate(CELLS(id)%x, CELLS(id)%z, act_mutation_rate)
        
        call random_number(x)
        if (x < act_mutation_rate) then
           call random_number(x)
           If (x < MUTATION_DIVISION_RATE) then
              call random_number(x)
              qbook_length = len_trim(CELLS(id)%book(:))
              m = int(qbook_length*x)
              if (m/=0) then
                 mutation_char = random_code(1)
                 CELLS(id)%book(m:m) = mutation_char
                 MUTATION_COUNT = MUTATION_COUNT + 1
              end If
           else
              call random_number(x)
              qbook_length = len_trim(CELLS(id)%book(:))
              m = int(qbook_length*x)
              if (m/=0) then
                 mutation_bookmarker(:) = CELLS(id)%book(m:(m+BOOKMARKER_LENGTH-1))
                 CELLS(id)%bookmarker(:) = mutation_bookmarker(:)
                 MUTATION_COUNT = MUTATION_COUNT + 1
              end If
           end If
        end If


        If(CELLS(id)%out_data(ODUIO_TRANSF)<0.0d0) then
            return
        end If
        
        n = index(CELLS(id)%book, CELLS(id)%bookmarker)
        If(n/=0 .and. (n+BOOKMARKER_LENGTH<=BOOK_MAX_LENGTH)) then
            CELLS(id)%WAIT_FOR_CONNECT_F = 0
            CELLS(id)%WAIT_FOR_CONNECT_UI = 0
            CELLS(id)%WAIT_FOR_DISCONNECT_F = 0
            
            trans_flag = CELLS(id)%book((n+BOOKMARKER_LENGTH):(n+BOOKMARKER_LENGTH))
            bp = n + BOOKMARKER_LENGTH + 1
            qbook_length = len_trim(CELLS(id)%book(bp:))
            If(index(EXPANSION_FLAG, trans_flag)/=0) then
                act_NBLTE = NBLTE + nint(decode(CELLS(id)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                If(act_NBLTE<=qbook_length) then
                    call trans_cell_expansion(id, bp, CELLS(id)%book, act_NBLTE, act_mutation_rate)
                end If
            else If(index(CONNECT_FLAG, trans_flag)/=0) then
                act_NBLTC = NBLTC + nint(decode(CELLS(id)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                If(act_NBLTC<=qbook_length) then
                    Do k=1, NOCC
                        If(CELLS(id)%CCF(k)==0) then
                            exit
                        end If
                    end Do
                    If(k==(NOCC+1)) then
                        return
                    end If
                    CELLS(id)%WAIT_FOR_CONNECT_F = 1
                    CELLS(id)%WAIT_FOR_CONNECT_UI = k
                end If
            else If(index(DISCONNECT_FLAG, trans_flag)/=0) then
                act_NBLTD = NBLTD + nint(decode(CELLS(id)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                If(act_NBLTD<=qbook_length) then
                   CELLS(id)%WAIT_FOR_DISCONNECT_F = 1
                end If
            else If(index(TURN_BOOKMARKER_FLAG, trans_flag)/=0) then
                TURN_BOOKMARKER_COUNT = TURN_BOOKMARKER_COUNT + 1
                act_NBLTT = NBLTT + nint(decode(CELLS(id)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                If(act_NBLTT<=qbook_length) then
                   bp = bp + nint(decode(CELLS(id)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                   bpfb = 0
                   Do i=1, BOOKMARKER_LENGTH
                      CELLS(id)%bookmarker(i:i) = CELLS(id)%book((bp+bpfb):(bp+bpfb))
                      bpfb = bpfb + 2
                   end Do
                   bp = bp + bpfb - 2 + 1
                   CELLS(id)%bookmarker_advance = CELLS(id)%book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))
                end If
            end If
         end If

    end subroutine trans_cell_flag_processing

    subroutine trans_cell_expansion(id, bp, in_book, act_NBLTE, amr)
        Character(len=*), intent(in) :: in_book
        Character(len=BOOK_MAX_LENGTH) :: book
        Character(len=BOOKMARKER_LENGTH) :: bc_first_bm, bc_last_bm 
        Character(len=1) :: mutation_char
        Integer :: act_NBLTE
        Integer :: MUTATION_FLAG
        Integer :: id, bp, nid, bpfb
        Integer :: i, j, k, m
        Integer :: bc_first, bc_last
        Double precision :: amr
        Double precision :: ru, rv
        Double precision :: r, x, y, z, l
        Double precision :: E_inf
        Double precision :: gene_cost


        book(:) = in_book(:)

        MUTATION_FLAG = 0
        call random_number(r)
        If (r < MUTATION_COEFF4EXPANSION*amr) then
           call random_number(r)
           m = int((act_NBLTE-1)*r) + 1
           if (m/=0) then
              mutation_char = random_code(1)
              book(bp+m:bp+m) = mutation_char
              MUTATION_FLAG = 1
           end If
        end If

        Do k=1, NOCC
            If(CELLS(id)%CCF(k)==0) then
                exit
            end If
        end Do

        If(k==(NOCC+1)) then
            return
        end If

        nid = NOT_CALC_CL(1)

        CELLS(nid)%exist = 1

        CELLS(nid)%ar = BDAC*decode(book(bp:(bp+BDAD-1)), BDAS)/0.4095d0
        bp = bp + BDAD
        CELLS(nid)%ag = BDAC*decode(book(bp:(bp+BDAD-1)), BDAS)/0.4095d0
        bp = bp + BDAD
        CELLS(nid)%ab = BDAC*decode(book(bp:(bp+BDAD-1)), BDAS)/0.4095d0
        bp = bp + BDAD

        CELLS(nid)%cr = 1.0d0 - CELLS(nid)%ar
        CELLS(nid)%cg = 1.0d0 - CELLS(nid)%ag
        CELLS(nid)%cb = 1.0d0 - CELLS(nid)%ab

        CELLS(nid)%IOLr = BDIC*decode(book(bp:(bp+BDID-1)), BDIS)
        If(CELLS(nid)%IOLr < MIN_I) then
           CELLS(nid)%IOLr = MIN_I
        end If
        bp = bp + BDID
        CELLS(nid)%IOLg = BDIC*decode(book(bp:(bp+BDID-1)), BDIS)
        If(CELLS(nid)%IOLg < MIN_I) then
           CELLS(nid)%IOLg = MIN_I
        end If
        bp = bp + BDID
        CELLS(nid)%IOLb = BDIC*decode(book(bp:(bp+BDID-1)), BDIS)
        If(CELLS(nid)%IOLb < MIN_I) then
           CELLS(nid)%IOLb = MIN_I
        end If
        bp = bp + BDID

        CELLS(nid)%m = BDMC*decode(book(bp:(bp+BDMD-1)), BDMS)
        If(CELLS(nid)%m < MIN_M) then
           CELLS(nid)%m = MIN_M
        else If(MAX_M < CELLS(nid)%m) then
           CELLS(nid)%m = MAX_M
        end If
        bp = bp + BDMD
        CELLS(nid)%r = BDRC*decode(book(bp:(bp+BDRD-1)), BDRS)
        If(CELLS(nid)%r < MIN_R) then
           CELLS(nid)%r = MIN_R
        end If
        bp = bp + BDRD

        l = SPRING_LIMIT_C*(CELLS(id)%r + CELLS(nid)%r)
        
        E_inf = calc_energy_inf(CELLS(nid)%r, CELLS(nid)%ar, CELLS(nid)%ag, CELLS(nid)%ab, NOCC)
        
        gene_cost = GENE_COST_C*E_inf
        
        If(CELLS(id)%E<gene_cost) then
            CELLS(nid)%exist = 0

            CELLS(nid)%ar = 0.0d0
            CELLS(nid)%ag = 0.0d0
            CELLS(nid)%ab = 0.0d0

            CELLS(nid)%cr = 0.0d0
            CELLS(nid)%cg = 0.0d0
            CELLS(nid)%cb = 0.0d0

            CELLS(nid)%IOLr = 0.0d0
            CELLS(nid)%IOLg = 0.0d0
            CELLS(nid)%IOLb = 0.0d0

            CELLS(nid)%m = 0.0d0
            CELLS(nid)%r = 0.0d0
            return
        end If

        EXPANSION_COUNT = EXPANSION_COUNT + 1
        if(MUTATION_FLAG==1) then
           MUTATION_COUNT = MUTATION_COUNT + 1
        end if

        call random_number(ru)
        call random_number(rv)

        z = -2.0d0*ru + 1
        x = sqrt(1.0d0 - z**2)*cos(2.0d0*PI*rv)
        y = sqrt(1.0d0 - z**2)*sin(2.0d0*PI*rv)

        x = l*x
        y = l*y
        z = l*z
        
        CELLS(nid)%x = x + CELLS(id)%x
        CELLS(nid)%y = y + CELLS(id)%y
        CELLS(nid)%z = z + CELLS(id)%z

        CELLS(nid)%vx = CELLS(id)%vx
        CELLS(nid)%vy = CELLS(id)%vy
        CELLS(nid)%vz = CELLS(id)%vz

        CELLS(nid)%fx = 0.0d0
        CELLS(nid)%fy = 0.0d0
        CELLS(nid)%fz = 0.0d0

        CELLS(nid)%E = gene_cost

        CELLS(nid)%alpha = 0.0d0

        CELLS(nid)%ID  = nid
        CELLS(nid)%age = 2

        CELLS(nid)%in_data(:) = 0.0d0
        CELLS(nid)%out_data(:) = 0.0d0

        Do i=1, NOHU
            Do j=1, (NOIU+1)
                CELLS(nid)%w1(i,j) = BDWC*decode(book(bp:(bp+BDWD-1)), BDWS)
                bp = bp + BDWD
            end Do
        end Do

        Do i=1, NOOU
            Do j=1, (NOHU+1)
                CELLS(nid)%w2(i,j) = BDWC*decode(book(bp:(bp+BDWD-1)), BDWS)
                bp = bp + BDWD
            end Do
        end Do

        CELLS(nid)%KOCC(:) = 0.0d0
        CELLS(nid)%LOCC(:) = 0.0d0
        CELLS(nid)%SOCC(:) = 0.0d0

        CELLS(nid)%CCF(:) = 0
        CELLS(nid)%IOCC(:) = 0
        CELLS(nid)%UIOCC(:) = 0

        CELLS(nid)%HIT_BLOCK_AF = 1
        CELLS(nid)%HIT_CELL_AF = 1
        CELLS(nid)%SPRING_AF = 1
        CELLS(nid)%MECHANICS_AF = 1
        CELLS(nid)%EAT_AF = 0
        CELLS(nid)%FUSION_AF = 0
        CELLS(nid)%LIGHT_AF = 0
        CELLS(nid)%INFO_TRANS_F = 0
        CELLS(nid)%NEURAL_NETWORK_F = 1
        CELLS(nid)%WAIT_FOR_CONNECT_F = 0
        CELLS(nid)%WAIT_FOR_CONNECT_UI = 0
        CELLS(nid)%WAIT_FOR_DISCONNECT_F = 0
        CELLS(nid)%ALONE_F = 1

        bpfb = 0
        Do i=1, BOOKMARKER_LENGTH
           bc_first_bm(i:i) = book((bp+bpfb):(bp+bpfb))
           bpfb = bpfb + 2
        end Do
        bp = bp + bpfb - 2 + 1

        bpfb = 0
        Do i=1, BOOKMARKER_LENGTH
           bc_last_bm(i:i) = book((bp+bpfb):(bp+bpfb))
           bpfb = bpfb + 2
        end Do
        bp = bp + bpfb - 2 + 1

        bpfb = 0
        Do i=1, BOOKMARKER_LENGTH
           CELLS(nid)%bookmarker(i:i) = book((bp+bpfb):(bp+bpfb))
           bpfb = bpfb + 2
        end Do
        bp = bp + bpfb - 2 + 1

        CELLS(nid)%bookmarker_advance = book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))
        bp = bp + 1

        bp = bp + nint(decode(CELLS(id)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
        
        bpfb = 0
        Do i=1, BOOKMARKER_LENGTH
           CELLS(id)%bookmarker(i:i) = book((bp+bpfb):(bp+bpfb))
           bpfb = bpfb + 2
        end Do
        bp = bp + bpfb - 2 + 1

        CELLS(id)%bookmarker_advance = book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))
        bp = bp + 1

        bc_first = index(book, bc_first_bm)
        bc_last = index(book, bc_last_bm, back=.true.)
        If(bc_first==0 .or. bc_last==0) then
            CELLS(nid)%book(:) = ''
        else
            CELLS(nid)%book(1:(bc_last-bc_first+1+(BOOKMARKER_LENGTH-1))) = book(bc_first:bc_last+(BOOKMARKER_LENGTH-1))
        end If

        CELLS(nid)%KOCC(1) = INIT_K
        CELLS(nid)%LOCC(1) = SPRING_LIMIT_C*CELLS(nid)%r
        CELLS(nid)%SOCC(1) = INIT_S

        CELLS(nid)%CCF(1) = 1
        CELLS(nid)%IOCC(1) = id
        CELLS(nid)%UIOCC(1) = k

        CELLS(id)%KOCC(k) = INIT_K
        CELLS(id)%LOCC(k) = SPRING_LIMIT_C*CELLS(id)%r
        CELLS(id)%SOCC(k) = INIT_S

        CELLS(id)%CCF(k) = 1
        CELLS(id)%IOCC(k) = nid
        CELLS(id)%UIOCC(k) = 1
        call add_BACK_CONN_CL(id, k, nid, 1)

        call delete_NOT_CALC_CL()
        call add_BACK_CALC_CL(nid)

        CELLS(id)%E = CELLS(id)%E - gene_cost

    end subroutine trans_cell_expansion

    subroutine light_calculation()
        Integer :: id, CALC_CLn
        Do CALC_CLn=1, CALC_CLL
            id = CALC_CL(CALC_CLn)
            If(CELLS(id)%LIGHT_AF==1) then
                call sub_light_calculation(id)
            end If
        end Do
    end subroutine light_calculation

    subroutine sub_light_calculation(id)
        Integer :: id
        Integer :: i, j, N
        Double precision :: x, y, z, r
        Double precision :: vc, nvx, nvy, nvz
        Double precision :: ru, rv
        Double precision :: Ir, Ig, Ib
        Double precision :: lost_energy

        N = nint(INFO_TRANS_EMI_C*(CELLS(id)%r**2))

        Ir = CELLS(id)%IOLr
        Ig = CELLS(id)%IOLg
        Ib = CELLS(id)%IOLb
        lost_energy = ECEOL_E2L*(Ir + Ig + Ib)

        Do i=1, N
            If(CELLS(id)%E<lost_energy) then
                exit
            end If
            
            call random_number(ru)
            call random_number(rv)

            z = -2.0d0*ru + 1
            x = sqrt(1.0d0 - z**2)*cos(2.0d0*PI*rv)
            y = sqrt(1.0d0 - z**2)*sin(2.0d0*PI*rv)

            r = 1.01d0*(CELLS(id)%r + INFO_TRANS_R)

            vc = CELLS(id)%out_data(ODUIO_LIGHT)
            
            nvx = x
            nvy = y
            nvz = z

            x = r*x
            y = r*y
            z = r*z
            
            x = x + CELLS(id)%x
            y = y + CELLS(id)%y
            z = z + CELLS(id)%z

            j = NOT_CALC_CL(1)
            call delete_NOT_CALC_CL()
            call add_BACK_CALC_CL(j)
            call create_information_transmitter(j, x, y, z, vc, nvx, nvy, nvz, Ir, Ig, Ib)
            CELLS(id)%E = CELLS(id)%E - lost_energy

        end Do

    end subroutine sub_light_calculation

    subroutine create_information_transmitter(id, x, y, z, vc, nvx, nvy, nvz, Ir, Ig, Ib)
        Integer :: id
        Double precision :: x, y, z
        Double precision :: vc, nvx, nvy, nvz
        Double precision :: Ir, Ig, Ib, Imax

        Imax = Ir
        If(Imax<Ig) then
            Imax = Ig
        end If
        If(Imax<Ib) then
            Imax = Ib
        end If

        CELLS(id)%exist = 1
        CELLS(id)%x = x
        CELLS(id)%y = y
        CELLS(id)%z = z

        CELLS(id)%nx = 0
        CELLS(id)%ny = 0
        CELLS(id)%nz = 0

        CELLS(id)%vx = vc*INFO_TRANS_SPEED*nvx
        CELLS(id)%vy = vc*INFO_TRANS_SPEED*nvy
        CELLS(id)%vz = vc*INFO_TRANS_SPEED*nvz

        CELLS(id)%fx = 0.0d0
        CELLS(id)%fy = 0.0d0
        CELLS(id)%fz = 0.0d0

        CELLS(id)%cr = Ir/Imax
        CELLS(id)%cg = Ig/Imax
        CELLS(id)%cb = Ib/Imax

        CELLS(id)%ar = 1.0d0 - Ir/Imax
        CELLS(id)%ag = 1.0d0 - Ib/Imax
        CELLS(id)%ab = 1.0d0 - Ig/Imax

        CELLS(id)%IOLr = Ir
        CELLS(id)%IOLg = Ig
        CELLS(id)%IOLb = Ib

        CELLS(id)%m = INFO_TRANS_M
        CELLS(id)%r = INFO_TRANS_R
        CELLS(id)%E = Ir + Ig + Ib

        CELLS(id)%alpha = 0.0d0

        CELLS(id)%ID  = id
        CELLS(id)%age = 0

        CELLS(id)%in_data(:) = 0.0d0

        CELLS(id)%out_data(:) = 0.0d0

        CELLS(id)%w1(:,:) = 0.0d0

        CELLS(id)%w2(:,:) = 0.0d0

        CELLS(id)%KOCC(:) = 0.0d0

        CELLS(id)%LOCC(:) = 0.0d0

        CELLS(id)%SOCC(:) = 0.0d0

        CELLS(id)%IOCC(:) = 0

        CELLS(id)%UIOCC(:) = 0

        CELLS(id)%CCF(:) = 0

        CELLS(id)%HIT_BLOCK_AF = 1
        CELLS(id)%HIT_CELL_AF = 1
        CELLS(id)%SPRING_AF = 0
        CELLS(id)%MECHANICS_AF = 0
        CELLS(id)%EAT_AF = 0
        CELLS(id)%FUSION_AF = 0
        CELLS(id)%LIGHT_AF = 0
        CELLS(id)%INFO_TRANS_F = 1
        CELLS(id)%NEURAL_NETWORK_F = 0
        CELLS(id)%WAIT_FOR_CONNECT_F = 0
        CELLS(id)%WAIT_FOR_CONNECT_UI = 0
        CELLS(id)%WAIT_FOR_DISCONNECT_F = 0
        CELLS(id)%ALONE_F = 1

    end subroutine create_information_transmitter

    subroutine free_cell(id)
        Integer :: fp=12
        Integer :: i, id, id2

        if(TRACE_FLAG==1) then
           open(fp, file="./trace_free.ini", status="old", position="append")
           write(fp, *) WORLD_STEP, id
           close(fp)
        end if
        
        CELLS(id)%book(:) = ''
        CELLS(id)%bookmarker(:) = ''
        CELLS(id)%bookmarker_advance(:) = ''

        CELLS(id)%exist = 0
        CELLS(id)%x = 0.0d0
        CELLS(id)%y = 0.0d0
        CELLS(id)%z = 0.0d0

        CELLS(id)%nx = 0
        CELLS(id)%ny = 0
        CELLS(id)%nz = 0

        CELLS(id)%vx = 0.0d0
        CELLS(id)%vy = 0.0d0
        CELLS(id)%vz = 0.0d0

        CELLS(id)%fx = 0.0d0
        CELLS(id)%fy = 0.0d0
        CELLS(id)%fz = 0.0d0

        CELLS(id)%cr = 0.0d0
        CELLS(id)%cg = 0.0d0
        CELLS(id)%cb = 0.0d0

        CELLS(id)%ar = 0.0d0
        CELLS(id)%ag = 0.0d0
        CELLS(id)%ab = 0.0d0

        CELLS(id)%IOLr = 0.0d0
        CELLS(id)%IOLg = 0.0d0
        CELLS(id)%IOLb = 0.0d0

        CELLS(id)%m = 0.0d0
        CELLS(id)%r = 0.0d0
        CELLS(id)%E = 0.0d0

        CELLS(id)%alpha = 0.0d0

        CELLS(id)%ID  = id
        CELLS(id)%age = 0

        CELLS(id)%in_data(:) = 0.0d0

        CELLS(id)%out_data(:) = 0.0d0

        CELLS(id)%w1(:,:) = 0.0d0

        CELLS(id)%w2(:,:) = 0.0d0

        CELLS(id)%KOCC(:) = 0.0d0

        CELLS(id)%LOCC(:) = 0.0d0

        CELLS(id)%SOCC(:) = 0.0d0

        Do i=1, NOCC
            If(CELLS(id)%CCF(i)==1) then
                id2 = CELLS(id)%IOCC(i)
                call delete_BACK_CONN_CL(id, id2)
            end If
        end Do

        CELLS(id)%IOCC(:) = 0

        CELLS(id)%UIOCC(:) = 0

        CELLS(id)%CCF(:) = 0

        CELLS(id)%HIT_BLOCK_AF = 0
        CELLS(id)%HIT_CELL_AF = 0
        CELLS(id)%SPRING_AF = 0
        CELLS(id)%MECHANICS_AF = 0
        CELLS(id)%EAT_AF = 0
        CELLS(id)%FUSION_AF = 0
        CELLS(id)%LIGHT_AF = 0
        CELLS(id)%INFO_TRANS_F = 0
        CELLS(id)%NEURAL_NETWORK_F = 0
        CELLS(id)%WAIT_FOR_CONNECT_F = 0
        CELLS(id)%WAIT_FOR_CONNECT_UI = 0
        CELLS(id)%WAIT_FOR_DISCONNECT_F = 0
        CELLS(id)%ALONE_F = 0

    end subroutine free_cell

    subroutine add_BACK_CONN_CL(id1, k1, id2, k2)
        Integer :: id1, id2
        Integer :: k1, k2

        BACK_CONN_CLL = BACK_CONN_CLL + 1
        If(CONN_CLL_MAX<BACK_CONN_CLL) then
           write(*, *) 'error from add_BACK_CONN_CL'
           call emergency_stop()
        end If

        BACK_CONN_CL(BACK_CONN_CLL, 1, 1) = id1
        BACK_CONN_CL(BACK_CONN_CLL, 1, 2) = k1
        BACK_CONN_CL(BACK_CONN_CLL, 2, 1) = id2
        BACK_CONN_CL(BACK_CONN_CLL, 2, 2) = k2

    end subroutine add_BACK_CONN_CL

    subroutine delete_BACK_CONN_CL(id1, id2)
        Integer :: id1, id2
        Integer :: k1, k2
        Integer :: c
        Integer, allocatable :: backup_list(:, :, :)

        k1 = 0
        k2 = 0
        
        Do c=1, BACK_CONN_CLL
            If(BACK_CONN_CL(c,1,1)==id1) then
                If(BACK_CONN_CL(c,2,1)==id2) then
                    k1 = BACK_CONN_CL(c,1,2)
                    k2 = BACK_CONN_CL(c,2,2)
                    exit
                end If
            else If(BACK_CONN_CL(c,1,1)==id2) then
                If(BACK_CONN_CL(c,2,1)==id1) then
                    k1 = BACK_CONN_CL(c,2,2)
                    k2 = BACK_CONN_CL(c,1,2)
                    exit
                end If
            end If
        end Do

        if(k1<=0 .or. k2<=0) then 
            write(*,*) 'error from delete_BACK_CONN_CL'
            write(*,*) '  k1=', k1
            write(*,*) '  k2=', k2
            call emergency_stop()
        end If

        If(c==(BACK_CONN_CLL+1)) then
            write(*,*) 'error from delete_BACK_CONN_CL'
            call emergency_stop()
        end If

        CELLS(id1)%KOCC(k1) = 0.0d0
        CELLS(id1)%LOCC(k1) = 0.0d0
        CELLS(id1)%SOCC(k1) = 0.0d0

        CELLS(id1)%CCF(k1) = 0
        CELLS(id1)%IOCC(k1) = 0
        CELLS(id1)%UIOCC(k1) = 0

        CELLS(id2)%KOCC(k2) = 0.0d0
        CELLS(id2)%LOCC(k2) = 0.0d0
        CELLS(id2)%SOCC(k2) = 0.0d0

        CELLS(id2)%CCF(k2) = 0
        CELLS(id2)%IOCC(k2) = 0
        CELLS(id2)%UIOCC(k2) = 0

        BACK_CONN_CLL = BACK_CONN_CLL - 1

        allocate(backup_list(BACK_CONN_CLL, 2, 2))
        If(c/=1) then
            backup_list(1:(c-1), :, :) = BACK_CONN_CL(1:(c-1), :, :)
        end If
        backup_list(c:BACK_CONN_CLL, :, :) = BACK_CONN_CL((c+1):(BACK_CONN_CLL+1), :, :)
        BACK_CONN_CL(1:BACK_CONN_CLL, :, :) = backup_list(1:BACK_CONN_CLL, :, :)
        BACK_CONN_CL((BACK_CONN_CLL+1), :, :) = 0
        deallocate(backup_list)

    end subroutine delete_BACK_CONN_CL

    subroutine add_BACK_CALC_CL(id)
        Integer :: id

        BACK_CALC_CLL = BACK_CALC_CLL + 1
        BACK_CALC_CL(BACK_CALC_CLL) = id

    end subroutine add_BACK_CALC_CL

    subroutine delete_BACK_CALC_CL(id)
        Integer :: id
        Integer :: c
        Integer, allocatable :: backup_list(:)

        Do c=1, BACK_CALC_CLL
            If(BACK_CALC_CL(c)==id) then
                exit
            end If
        end Do

        If(c==(BACK_CALC_CLL+1)) then
            write(*,*) 'error from delete_BACK_CALC_CL'
            call emergency_stop()
        end If

        BACK_CALC_CLL = BACK_CALC_CLL - 1

        allocate(backup_list(BACK_CALC_CLL))
        If(c/=1) then
            backup_list(1:(c-1)) = BACK_CALC_CL(1:(c-1))
        end If
        backup_list(c:BACK_CALC_CLL) = BACK_CALC_CL((c+1):(BACK_CALC_CLL+1))
        BACK_CALC_CL(1:BACK_CALC_CLL) = backup_list(1:BACK_CALC_CLL)
        BACK_CALC_CL((BACK_CALC_CLL+1)) = 0
        deallocate(backup_list)

    end subroutine delete_BACK_CALC_CL

    subroutine add_NEW_NOT_CALC_CL(id)
        Integer :: id

        NEW_NOT_CALC_CLL = NEW_NOT_CALC_CLL + 1
        If(NUMBER_OF_CELL<=NEW_NOT_CALC_CLL) then
            write(*,*) 'error from add_NEW_NOT_CALC_CL'
            call emergency_stop()
        end If
        NOT_CALC_CL(NEW_NOT_CALC_CLL) = id

    end subroutine add_NEW_NOT_CALC_CL

    subroutine delete_NOT_CALC_CL()
        Integer, allocatable :: backup_list(:)

        NOT_CALC_CLL = NOT_CALC_CLL - 1
        NEW_NOT_CALC_CLL = NEW_NOT_CALC_CLL - 1

        If(NOT_CALC_CLL<=0) then
            write(*,*) 'error from delete_NOT_CALC_CL'
            call emergency_stop()
        end If

        allocate(backup_list(NOT_CALC_CLL))
        backup_list(1:NOT_CALC_CLL) = NOT_CALC_CL(2:(NOT_CALC_CLL+1))
        NOT_CALC_CL(1:NOT_CALC_CLL) = backup_list(1:NOT_CALC_CLL)
        NOT_CALC_CL((NOT_CALC_CLL+1)) = 0
        deallocate(backup_list)

    end subroutine delete_NOT_CALC_CL

    subroutine cell_hit_block()
        Integer :: CALC_CLn, id

        !$omp parallel 
        !$omp do private(id, CALC_CLn)
        Do CALC_CLn=1, CALC_CLL
            id = CALC_CL(CALC_CLn)
            If(CELLS(id)%HIT_BLOCK_AF==1) then
                call sub_cell_hit_block(id)
            end If
        end Do
        !$omp end do
        !$omp end parallel
        
    end subroutine cell_hit_block

    subroutine sub_cell_hit_block(id)
        Integer :: id
        Integer :: i, j, k
        Integer :: cell_nx, cell_ny, cell_nz
        Double precision :: x, y, z, vx, vy, vz, cellr
        Double precision :: fx, fy, fz
        Double precision :: bs, bt, bu
        Double precision :: nx, ny, nz

        x = CELLS(id)%x
        y = CELLS(id)%y
        z = CELLS(id)%z
        cellr = CELLS(id)%r

        vx = CELLS(id)%vx
        vy = CELLS(id)%vy
        vz = CELLS(id)%vz

        cell_nx = CELLS(id)%nx
        cell_ny = CELLS(id)%ny
        cell_nz = CELLS(id)%nz

        fx = 0.0d0
        fy = 0.0d0
        fz = 0.0d0

        ! !  <- must
        ! !! <- can be omit

        
        !!--------------------------B------------------------------
        i = cell_nx
        j = cell_ny - 1
        k = cell_nz - 1

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%y
            bt = FIELD(i,j,k)%z
            !call sub_sub_cell_hit_block2(j, k, y, z, vy, vz, vx, cellr, fy, fz, fx, bs, bt, 0.5d0, 0.5d0, id)
            call sub_sub_cell_hit_block2(y, z, vy, vz, vx, cellr, fy, fz, fx, bs, bt, 0.5d0, 0.5d0, id)
        end If

        !!--------------------------D------------------------------
        i = cell_nx - 1
        j = cell_ny - 1
        k = cell_nz

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            bt = FIELD(i,j,k)%y
            !call sub_sub_cell_hit_block2(i, j, x, y, vy, vz, vx, cellr, fx, fy, fz, bs, bt, 0.5d0, 0.5d0, id)
            call sub_sub_cell_hit_block2(x, y, vy, vz, vx, cellr, fx, fy, fz, bs, bt, 0.5d0, 0.5d0, id)
        end If

        !--------------------------E------------------------------
        i = cell_nx
        j = cell_ny - 1
        k = cell_nz

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%y
            !call sub_sub_cell_hit_block1(j, y, vy, vx, vz, cellr, fy, fx, fz, bs, 0.5d0, id)
            call sub_sub_cell_hit_block1(y, vy, vx, vz, cellr, fy, fx, fz, bs, 0.5d0, id)
        end If
        
        !!--------------------------F------------------------------
        i = cell_nx + 1
        j = cell_ny - 1
        k = cell_nz

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            bt = FIELD(i,j,k)%y
            !call sub_sub_cell_hit_block2(i, j, x, y, vy, vz, vx, cellr, fx, fy, fz, bs, bt, -0.5d0, 0.5d0, id)
            call sub_sub_cell_hit_block2(x, y, vy, vz, vx, cellr, fx, fy, fz, bs, bt, -0.5d0, 0.5d0, id)
        end If

        !!--------------------------H------------------------------
        i = cell_nx
        j = cell_ny - 1
        k = cell_nz + 1

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%y
            bt = FIELD(i,j,k)%z
            !call sub_sub_cell_hit_block2(j, k, y, z, vy, vz, vx, cellr, fy, fz, fx, bs, bt, 0.5d0, -0.5d0, id)
            call sub_sub_cell_hit_block2(y, z, vy, vz, vx, cellr, fy, fz, fx, bs, bt, 0.5d0, -0.5d0, id)
        end If

        !!--------------------------J------------------------------
        i = cell_nx - 1
        j = cell_ny
        k = cell_nz - 1

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            bt = FIELD(i,j,k)%z
            !call sub_sub_cell_hit_block2(i, k, x, z, vx, vz, vy, cellr, fx, fz, fy, bs, bt, 0.5d0, 0.5d0, id)
            call sub_sub_cell_hit_block2(x, z, vx, vz, vy, cellr, fx, fz, fy, bs, bt, 0.5d0, 0.5d0, id)
        end If
        
        !--------------------------K------------------------------
        i = cell_nx
        j = cell_ny
        k = cell_nz - 1

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%z
            !call sub_sub_cell_hit_block1(k, z, vz, vy, vx, cellr, fz, fy, fx, bs, 0.5d0, id)
            call sub_sub_cell_hit_block1(z, vz, vy, vx, cellr, fz, fy, fx, bs, 0.5d0, id)
        end If

        !!--------------------------L------------------------------
        i = cell_nx + 1
        j = cell_ny
        k = cell_nz - 1

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            bt = FIELD(i,j,k)%z
            !call sub_sub_cell_hit_block2(i, k, x, z, vx, vz, vy, cellr, fx, fz, fy, bs, bt, -0.5d0, 0.5d0, id)
            call sub_sub_cell_hit_block2(x, z, vx, vz, vy, cellr, fx, fz, fy, bs, bt, -0.5d0, 0.5d0, id)
        end If

        !--------------------------M------------------------------
        i = cell_nx - 1
        j = cell_ny
        k = cell_nz

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            !call sub_sub_cell_hit_block1(i, x, vx, vy, vz, cellr, fx, fy, fz, bs, 0.5d0, id)
            call sub_sub_cell_hit_block1(x, vx, vy, vz, cellr, fx, fy, fz, bs, 0.5d0, id)
        end If

        !--------------------------N------------------------------
        i = cell_nx
        j = cell_ny
        k = cell_nz

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            bt = FIELD(i,j,k)%y
            bu = FIELD(i,j,k)%z
            nx = x - bs
            ny = y - bt
            nz = z - bu
            !call sub_sub_cell_hit_blockn2(nx, ny, nz, vx, vy, vz, cellr, fx, fy, fz, id)
            call sub_sub_cell_hit_blockn2(nx, ny, nz, vx, vy, vz, fx, fy, fz, id)
        end If


        !--------------------------O------------------------------
        i = cell_nx + 1
        j = cell_ny
        k = cell_nz

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            !call sub_sub_cell_hit_block1(i, x, vx, vy, vz, cellr, fx, fy, fz, bs, -0.5d0, id)
            call sub_sub_cell_hit_block1(x, vx, vy, vz, cellr, fx, fy, fz, bs, -0.5d0, id)
        end If

        !!--------------------------P------------------------------
        i = cell_nx - 1
        j = cell_ny
        k = cell_nz + 1

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            bt = FIELD(i,j,k)%z
            !call sub_sub_cell_hit_block2(i, k, x, z, vx, vz, vy, cellr, fx, fz, fy, bs, bt, 0.5d0, -0.5d0, id)
            call sub_sub_cell_hit_block2(x, z, vx, vz, vy, cellr, fx, fz, fy, bs, bt, 0.5d0, -0.5d0, id)
        end If
        
        !--------------------------Q------------------------------
        i = cell_nx
        j = cell_ny
        k = cell_nz + 1

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%z
            !call sub_sub_cell_hit_block1(k, z, vz, vy, vx, cellr, fz, fy, fx, bs, -0.5d0, id)
            call sub_sub_cell_hit_block1(z, vz, vy, vx, cellr, fz, fy, fx, bs, -0.5d0, id)
        end If

        !!--------------------------R------------------------------
        i = cell_nx + 1
        j = cell_ny
        k = cell_nz + 1

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%x
            bt = FIELD(i,j,k)%z
            !call sub_sub_cell_hit_block2(i, k, x, z, vx, vz, vy, cellr, fx, fz, fy, bs, bt, -0.5d0, -0.5d0, id)
            call sub_sub_cell_hit_block2(x, z, vx, vz, vy, cellr, fx, fz, fy, bs, bt, -0.5d0, -0.5d0, id)
        end If
        
        !--------------------------W------------------------------
        i = cell_nx
        j = cell_ny + 1
        k = cell_nz

        If(1<=FIELD(i,j,k)%wall) then
            bs = FIELD(i,j,k)%y
            !call sub_sub_cell_hit_block1(j, y, vy, vx, vz, cellr, fy, fx, fz, bs, -0.5d0, id)
            call sub_sub_cell_hit_block1(y, vy, vx, vz, cellr, fy, fx, fz, bs, -0.5d0, id)
        end If

        CELLS(id)%fx = CELLS(id)%fx + fx
        CELLS(id)%fy = CELLS(id)%fy + fy
        CELLS(id)%fz = CELLS(id)%fz + fz

    end subroutine sub_cell_hit_block

    subroutine sub_sub_cell_hit_block1(s, vs, vt, vu, cellr, fs, ft, fu, bs, hls, id)
        Integer :: id
        Double precision :: s, vs, vt, vu, cellr, r, hls
        Double precision :: bs
        Double precision :: ts
        Double precision :: fs, ft, fu
        Double precision :: eff_block_emd_force_r
        Double precision :: eff_block_emd_force_k

        If(CELLS(id)%INFO_TRANS_F==1) then
           eff_block_emd_force_r = INFO_TRANS_BLOCK_EMD_FORCE_R*BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = INFO_TRANS_BLOCK_EMD_FORCE_K*BLOCK_EMD_FORCE_K
        else
           eff_block_emd_force_r = BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = BLOCK_EMD_FORCE_K
        end If

        r = cellr - abs(s - bs - hls)
        If(0.0d0<r) then
            ts = s - bs
            fs = fs + (ts/abs(ts))*eff_block_emd_force_k*r - eff_block_emd_force_r*vs
            ft = ft - eff_block_emd_force_r*vt
            fu = fu - eff_block_emd_force_r*vu
            If(CELLS(id)%INFO_TRANS_F==1) then
                CELLS(id)%IOLr = CELLS(id)%IOLr*(1.0d0 - WALL_ar)
                CELLS(id)%IOLg = CELLS(id)%IOLg*(1.0d0 - WALL_ag)
                CELLS(id)%IOLb = CELLS(id)%IOLb*(1.0d0 - WALL_ab)
                CELLS(id)%E = CELLS(id)%IOLr + CELLS(id)%IOLg + CELLS(id)%IOLb
            end If
        end If

    end subroutine sub_sub_cell_hit_block1

    subroutine sub_sub_cell_hit_block2(s, t, vs, vt, vu, cellr, fs, ft, fu, bs, bt, hls, hlt, id)
        Integer :: id
        Double precision :: s, t, vs, vt, vu, cellr, tr, r, d, hls, hlt
        Double precision :: bs, bt
        Double precision :: ts, tt
        Double precision :: fs, ft, fu
        Double precision :: eff_block_emd_force_r
        Double precision :: eff_block_emd_force_k

        If(CELLS(id)%INFO_TRANS_F==1) then
           eff_block_emd_force_r = INFO_TRANS_BLOCK_EMD_FORCE_R*BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = INFO_TRANS_BLOCK_EMD_FORCE_K*BLOCK_EMD_FORCE_K
        else
           eff_block_emd_force_r = BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = BLOCK_EMD_FORCE_K
        end If

        ts = s - bs
        tt = t - bt

        r = cellr - sqrt((ts - hls)**2 + (tt - hlt)**2)

        If(0.0d0<r) then
            tr = sqrt(ts**2 + tt**2)
            d = eff_block_emd_force_k*r
            fs = fs + d*(ts/tr) - eff_block_emd_force_r*vs
            ft = ft + d*(tt/tr) - eff_block_emd_force_r*vt
            fu = fu - eff_block_emd_force_r*vu
            If(CELLS(id)%INFO_TRANS_F==1) then
                CELLS(id)%IOLr = CELLS(id)%IOLr*(1.0d0 - WALL_ar)
                CELLS(id)%IOLg = CELLS(id)%IOLg*(1.0d0 - WALL_ag)
                CELLS(id)%IOLb = CELLS(id)%IOLb*(1.0d0 - WALL_ab)
                CELLS(id)%E = CELLS(id)%IOLr + CELLS(id)%IOLg + CELLS(id)%IOLb
            end If
        end If

    end subroutine sub_sub_cell_hit_block2

    subroutine sub_sub_cell_hit_block3(s, t, u, vs, vt, vu, cellr, fs, ft, fu, bs, bt, bu, hls, hlt, hlu, id)
        Integer :: id
        Double precision :: s, t, u, vs, vt, vu, cellr, tr, r, d, hls, hlt, hlu
        Double precision :: bs, bt, bu
        Double precision :: ts, tt, tu
        Double precision :: fs, ft, fu
        Double precision :: eff_block_emd_force_r
        Double precision :: eff_block_emd_force_k

        If(CELLS(id)%INFO_TRANS_F==1) then
           eff_block_emd_force_r = INFO_TRANS_BLOCK_EMD_FORCE_R*BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = INFO_TRANS_BLOCK_EMD_FORCE_K*BLOCK_EMD_FORCE_K
        else
           eff_block_emd_force_r = BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = BLOCK_EMD_FORCE_K
        end If
        
        ts = s - bs
        tt = t - bt
        tu = u - bu

        r = cellr - sqrt((ts - hls)**2 + (tt - hlt)**2 + (tu - hlu)**2)

        If(0.0d0<r) then
            tr = sqrt(ts**2 + tt**2 + tu**2)
            d = eff_block_emd_force_k*r
            fs = fs + d*(ts/tr) - eff_block_emd_force_r*vs
            ft = ft + d*(tt/tr) - eff_block_emd_force_r*vt
            fu = fu + d*(tu/tr) - eff_block_emd_force_r*vu
            If(CELLS(id)%INFO_TRANS_F==1) then
                CELLS(id)%IOLr = CELLS(id)%IOLr*(1.0d0 - WALL_ar)
                CELLS(id)%IOLg = CELLS(id)%IOLg*(1.0d0 - WALL_ag)
                CELLS(id)%IOLb = CELLS(id)%IOLb*(1.0d0 - WALL_ab)
                CELLS(id)%E = CELLS(id)%IOLr + CELLS(id)%IOLg + CELLS(id)%IOLb
            end If
        end If

    end subroutine sub_sub_cell_hit_block3

    subroutine sub_sub_cell_hit_blockn(s, vs, vt, vu, fs, ft, fu, bs, hls, id)
        Integer :: id
        Double precision :: s, vs, vt, vu
        Double precision :: r, hls
        Double precision :: bs
        Double precision :: fs, ft, fu
        Double precision :: eff_block_emd_force_r
        Double precision :: eff_block_emd_force_k

        If(CELLS(id)%INFO_TRANS_F==1) then
           eff_block_emd_force_r = INFO_TRANS_BLOCK_EMD_FORCE_R*BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = INFO_TRANS_BLOCK_EMD_FORCE_K*BLOCK_EMD_FORCE_K
        else
           eff_block_emd_force_r = BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = BLOCK_EMD_FORCE_K
        end If
        
        r = bs + hls - s
        fs = fs + eff_block_emd_force_k*r - eff_block_emd_force_r*vs
        ft = ft - eff_block_emd_force_r*vt
        fu = fu - eff_block_emd_force_r*vu
        If(CELLS(id)%INFO_TRANS_F==1) then
            CELLS(id)%IOLr = CELLS(id)%IOLr*(1.0d0 - WALL_ar)
            CELLS(id)%IOLg = CELLS(id)%IOLg*(1.0d0 - WALL_ag)
            CELLS(id)%IOLb = CELLS(id)%IOLb*(1.0d0 - WALL_ab)
            CELLS(id)%E = CELLS(id)%IOLr + CELLS(id)%IOLg + CELLS(id)%IOLb
        end If
    end subroutine sub_sub_cell_hit_blockn

    subroutine sub_sub_cell_hit_blockn2(ns, nt, nu, vs, vt, vu, fs, ft, fu, id)
        Integer :: id
        Double precision :: ns, nt, nu
        Double precision :: vs, vt, vu
        Double precision :: r, blockr
        Double precision :: f, fs, ft, fu
        Double precision :: eff_block_emd_force_r
        Double precision :: eff_block_emd_force_k

        If(CELLS(id)%INFO_TRANS_F==1) then
           eff_block_emd_force_r = INFO_TRANS_BLOCK_EMD_FORCE_R*BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = INFO_TRANS_BLOCK_EMD_FORCE_K*BLOCK_EMD_FORCE_K
        else
           eff_block_emd_force_r = BLOCK_EMD_FORCE_R
           eff_block_emd_force_k = BLOCK_EMD_FORCE_K
        end If
        
        blockr = sqrt(3.0d0)/2.0d0
        r = sqrt(ns**2 + nt**2 + nu**2)
        f = eff_block_emd_force_k*(blockr - r)
        fs = fs + f*(ns/r) - eff_block_emd_force_r*vs
        ft = ft + f*(nt/r) - eff_block_emd_force_r*vt
        fu = fu + f*(nu/r) - eff_block_emd_force_r*vu
        If(CELLS(id)%INFO_TRANS_F==1) then
            CELLS(id)%IOLr = CELLS(id)%IOLr*(1.0d0 - WALL_ar)
            CELLS(id)%IOLg = CELLS(id)%IOLg*(1.0d0 - WALL_ag)
            CELLS(id)%IOLb = CELLS(id)%IOLb*(1.0d0 - WALL_ab)
            CELLS(id)%E = CELLS(id)%IOLr + CELLS(id)%IOLg + CELLS(id)%IOLb
        end If
    end subroutine sub_sub_cell_hit_blockn2

    subroutine cell_hit_cell()
        Integer :: CALC_CLn, id1, id2, uid1, uid2, i, j, k, m, n, bp, bpfb, sid, find_ccid_f
        Integer :: cell_nx, cell_ny, cell_nz, noc
        Double precision :: x, y, z, r, h, d
        Double precision :: lost_ene
    
        Do CALC_CLn=1, CALC_CLL
            id1 = CALC_CL(CALC_CLn)
            If(CELLS(id1)%HIT_CELL_AF==0) then
                cycle
            end If
            cell_nx = CELLS(id1)%nx - 1
            cell_ny = CELLS(id1)%ny - 1
            cell_nz = CELLS(id1)%nz - 1
            Do i=cell_nx, (cell_nx + 2)
                Do j=cell_ny, (cell_ny + 2)
                    Do k=cell_nz, (cell_nz + 2)
                        noc = FIELD(i,j,k)%NOC
                        Do m=1, noc
                            id2 = FIELD(i,j,k)%IOC(m)
                           
                            If(id2<id1) then
                               cycle
                            end If
    
                            If(CELLS(id1)%INFO_TRANS_F==1 .and. CELLS(id2)%INFO_TRANS_F==1) then
                                cycle
                            end If
                            If(CELLS(id1)%HIT_CELL_AF==0 .or. CELLS(id2)%HIT_CELL_AF==0) then
                                cycle
                            end If
                            If(id1<id2) then
                                x = CELLS(id1)%x - CELLS(id2)%x
                                y = CELLS(id1)%y - CELLS(id2)%y
                                z = CELLS(id1)%z - CELLS(id2)%z
                                r = CELLS(id1)%r + CELLS(id2)%r
                                h = sqrt(x**2 + y**2 + z**2)
                                d = r - h
                                If(0.0d0 < d) then
                                    call sub_cell_hit_cell(id1, id2, x, y, z, h, d)
                                end If
                                If(CELLS(id1)%WAIT_FOR_CONNECT_F==1 .and. CELLS(id2)%WAIT_FOR_CONNECT_F==1) then
                                    find_ccid_f = 0
                                    Do sid=1, NOCC
                                        If(CELLS(id1)%IOCC(sid)==id2) then
                                            find_ccid_f = 1
                                            exit
                                        end If
                                    end Do
                                    If(find_ccid_f==1) then
                                        cycle
                                    end If
                                    lost_ene = CONN_COST_C*INIT_K*r**2
                                    If((CELLS(id1)%E - lost_ene)<0.0d0 .or. (CELLS(id2)%E - lost_ene)<0.0d0) then
                                        cycle
                                    end If
    
                                    If(SPRING_CATCH_C*r<h) then
                                       cycle
                                    end If
                                     
                                    CONNECT_COUNT = CONNECT_COUNT + 1
                                    uid1 = CELLS(id1)%WAIT_FOR_CONNECT_UI
                                    uid2 = CELLS(id2)%WAIT_FOR_CONNECT_UI
    
                                    if(uid1<=0 .or. uid2<=0) then
                                       write(*, *) 'error from cell_hit_cell'
                                       write(*, *) '  uid1=', uid1
                                       write(*, *) '  uid2=', uid2
                                       call emergency_stop()
                                    end If
    
                                    call add_BACK_CONN_CL(id1, uid1, id2, uid2)

                                    CELLS(id1)%WAIT_FOR_CONNECT_F = 0
                                    CELLS(id1)%WAIT_FOR_CONNECT_UI = 0
    
                                    CELLS(id2)%WAIT_FOR_CONNECT_F = 0
                                    CELLS(id2)%WAIT_FOR_CONNECT_UI = 0
    
                                    CELLS(id1)%KOCC(uid1) = INIT_K
                                    CELLS(id1)%LOCC(uid1) = SPRING_LIMIT_C*CELLS(id1)%r
                                    CELLS(id1)%SOCC(uid1) = INIT_S
    
                                    CELLS(id1)%CCF(uid1) = 1
                                    CELLS(id1)%IOCC(uid1) = id2
                                    CELLS(id1)%UIOCC(uid1) = uid2
    
                                    CELLS(id2)%KOCC(uid2) = INIT_K
                                    CELLS(id2)%LOCC(uid2) = SPRING_LIMIT_C*CELLS(id2)%r
                                    CELLS(id2)%SOCC(uid2) = INIT_S
    
                                    CELLS(id2)%CCF(uid2) = 1
                                    CELLS(id2)%IOCC(uid2) = id1
                                    CELLS(id2)%UIOCC(uid2) = uid1
    
                                    CELLS(id1)%E = CELLS(id1)%E - lost_ene
                                    CELLS(id2)%E = CELLS(id2)%E - lost_ene
    
                                    bp = index(CELLS(id1)%book, CELLS(id1)%bookmarker) + BOOKMARKER_LENGTH + 1
                                    bpfb = 0
                                    Do n=1, BOOKMARKER_LENGTH
                                       CELLS(id1)%bookmarker(n:n) = CELLS(id1)%book((bp+bpfb):(bp+bpfb))
                                       bpfb = bpfb + 2
                                    end Do

                                    bp = bp + bpfb - 2 + 1
                                    CELLS(id1)%bookmarker_advance = CELLS(id1)%book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))
        
                                    CELLS(id1)%WAIT_FOR_CONNECT_F = 0
                                    CELLS(id1)%WAIT_FOR_CONNECT_UI = 0
    
                                    bp = index(CELLS(id2)%book, CELLS(id2)%bookmarker) + BOOKMARKER_LENGTH + 1
                                    bpfb = 0
                                    Do n=1, BOOKMARKER_LENGTH
                                       CELLS(id2)%bookmarker(n:n) = CELLS(id2)%book((bp+bpfb):(bp+bpfb))
                                       bpfb = bpfb + 2
                                    end Do
                                    bp = bp + bpfb - 2 + 1
                                    CELLS(id2)%bookmarker_advance = CELLS(id2)%book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))
    
                                    CELLS(id2)%WAIT_FOR_CONNECT_F = 0
                                    CELLS(id2)%WAIT_FOR_CONNECT_UI = 0
    
                                end If
                            end If
                        end Do
                    end Do
                end Do
            end Do
        end Do
    
    end subroutine cell_hit_cell
    
    subroutine cell_hit_cell2()
        Integer :: CALC_CLn, id1, id2, uid1, uid2, i, j, k, m, n, bp, bpfb, sid, find_ccid_f
        Integer :: cell_nx, cell_ny, cell_nz
        Double precision :: x, y, z, r, h, d
        Double precision :: lost_ene

        Do CALC_CLn=1, CALC_CLL
            id1 = CALC_CL(CALC_CLn)
            If(CELLS(id1)%HIT_CELL_AF==0) then
                cycle
            end If
            cell_nx = CELLS(id1)%nx - 1
            cell_ny = CELLS(id1)%ny - 1
            cell_nz = CELLS(id1)%nz - 1
            Do i=cell_nx, (cell_nx + 2)
                Do j=cell_ny, (cell_ny + 2)
                    Do k=cell_nz, (cell_nz + 2)
                        Do m=1, FIELD(i,j,k)%NOC

                            id2 = FIELD(i,j,k)%IOC(m)
                            If(id2<=id1) then
                               cycle
                            end If
                            
                            If(CELLS(id1)%INFO_TRANS_F==1 .and. CELLS(id2)%INFO_TRANS_F==1) then
                                cycle
                             end If
                             
                            If(CELLS(id1)%HIT_CELL_AF==0 .or. CELLS(id2)%HIT_CELL_AF==0) then
                                cycle
                            end If
                             
                            x = CELLS(id1)%x - CELLS(id2)%x
                            y = CELLS(id1)%y - CELLS(id2)%y
                            z = CELLS(id1)%z - CELLS(id2)%z
                            r = CELLS(id1)%r + CELLS(id2)%r
                            h = sqrt(x**2 + y**2 + z**2)
                            d = r - h
                            If(0.0d0 < d) then
                                call sub_cell_hit_cell(id1, id2, x, y, z, h, d)
                            end If
                            If(CELLS(id1)%WAIT_FOR_CONNECT_F==1 .and. CELLS(id2)%WAIT_FOR_CONNECT_F==1) then

                                find_ccid_f = 0
                                Do sid=1, NOCC
                                    If(CELLS(id1)%IOCC(sid)==id2) then
                                        find_ccid_f = 1
                                        exit
                                    end If
                                end Do
                                If(find_ccid_f==1) then
                                    cycle
                                end If
                                lost_ene = CONN_COST_C*INIT_K*r**2

                                If((CELLS(id1)%E - lost_ene)<0.0d0 .or. (CELLS(id2)%E - lost_ene)<0.0d0) then
                                    cycle
                                end If

                                If(SPRING_CATCH_C*r<h) then
                                   cycle
                                end If
                                 
                                CONNECT_COUNT = CONNECT_COUNT + 1
                                uid1 = CELLS(id1)%WAIT_FOR_CONNECT_UI
                                uid2 = CELLS(id2)%WAIT_FOR_CONNECT_UI

                                if(uid1<=0 .or. uid2<=0) then
                                   write(*, *) 'error from cell_hit_cell'
                                   write(*, *) '  uid1=', uid1
                                   write(*, *) '  uid2=', uid2
                                   call emergency_stop()
                                end If

                                call add_BACK_CONN_CL(id1, uid1, id2, uid2)

                                CELLS(id1)%WAIT_FOR_CONNECT_F = 0    
                                CELLS(id1)%WAIT_FOR_CONNECT_UI = 0

                                CELLS(id2)%WAIT_FOR_CONNECT_F = 0
                                CELLS(id2)%WAIT_FOR_CONNECT_UI = 0

                                CELLS(id1)%KOCC(uid1) = INIT_K
                                CELLS(id1)%LOCC(uid1) = SPRING_LIMIT_C*CELLS(id1)%r
                                CELLS(id1)%SOCC(uid1) = INIT_S

                                CELLS(id1)%CCF(uid1) = 1
                                CELLS(id1)%IOCC(uid1) = id2
                                CELLS(id1)%UIOCC(uid1) = uid2

                                CELLS(id2)%KOCC(uid2) = INIT_K
                                CELLS(id2)%LOCC(uid2) = SPRING_LIMIT_C*CELLS(id2)%r
                                CELLS(id2)%SOCC(uid2) = INIT_S

                                CELLS(id2)%CCF(uid2) = 1
                                CELLS(id2)%IOCC(uid2) = id1
                                CELLS(id2)%UIOCC(uid2) = uid1

                                CELLS(id1)%E = CELLS(id1)%E - lost_ene
                                CELLS(id2)%E = CELLS(id2)%E - lost_ene

                                bp = index(CELLS(id1)%book, CELLS(id1)%bookmarker) + BOOKMARKER_LENGTH + 1
                                bp = bp + nint(decode(CELLS(id1)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                                bpfb = 0
                                Do n=1, BOOKMARKER_LENGTH
                                   CELLS(id1)%bookmarker(n:n) = CELLS(id1)%book((bp+bpfb):(bp+bpfb))
                                   bpfb = bpfb + 2
                                end Do

                                bp = bp + bpfb - 2 + 1
                                CELLS(id1)%bookmarker_advance = CELLS(id1)%book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))

                                bp = index(CELLS(id2)%book, CELLS(id2)%bookmarker) + BOOKMARKER_LENGTH + 1
                                bp = bp + nint(decode(CELLS(id2)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                                bpfb = 0
                                Do n=1, BOOKMARKER_LENGTH
                                   CELLS(id2)%bookmarker(n:n) = CELLS(id2)%book((bp+bpfb):(bp+bpfb))
                                   bpfb = bpfb + 2
                                end Do
                                bp = bp + bpfb - 2 + 1
                                CELLS(id2)%bookmarker_advance = CELLS(id2)%book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))

                             end If

                        end Do
                    end Do
                end Do
            end Do
        end Do

    end subroutine cell_hit_cell2
      

    subroutine sub_cell_hit_cell(id1, id2, x, y, z, h, d)
        Integer :: id1, id2
        Integer :: cn1, cn2
        Double precision :: x, y, z, h, d, k
        Double precision :: fx, fy, fz
        Double precision :: EAT_COEFF

        k = CELL_EMD_FORCE_K*d

        fx = k*(x/h)
        fy = k*(y/h)
        fz = k*(z/h)

        CELLS(id1)%fx = CELLS(id1)%fx + fx - CELL_EMD_FORCE_R*CELLS(id1)%vx
        CELLS(id1)%fy = CELLS(id1)%fy + fy - CELL_EMD_FORCE_R*CELLS(id1)%vy
        CELLS(id1)%fz = CELLS(id1)%fz + fz - CELL_EMD_FORCE_R*CELLS(id1)%vz

        CELLS(id2)%fx = CELLS(id2)%fx - fx - CELL_EMD_FORCE_R*CELLS(id2)%vx
        CELLS(id2)%fy = CELLS(id2)%fy - fy - CELL_EMD_FORCE_R*CELLS(id2)%vy
        CELLS(id2)%fz = CELLS(id2)%fz - fz - CELL_EMD_FORCE_R*CELLS(id2)%vz

        If(CELLS(id1)%INFO_TRANS_F==1) then
            call update_info_trans_color(id1, id2)
        else If(CELLS(id2)%INFO_TRANS_F==1) then
            call update_info_trans_color(id2, id1)
        else
            CELLS(id1)%in_data(IDUIO_TOUCH) = CELLS(id1)%in_data(IDUIO_TOUCH) + CELL_RECEIVE_TOUCH
            CELLS(id2)%in_data(IDUIO_TOUCH) = CELLS(id2)%in_data(IDUIO_TOUCH) + CELL_RECEIVE_TOUCH
            
            cn1 = sum(CELLS(id1)%CCF(:))
            cn2 = sum(CELLS(id2)%CCF(:))
            If(cn1<cn2) then
                If(CELLS(id2)%EAT_AF==1) then
                    EAT_COEFF = ECEOE*(dble(cn2 - cn1)/dble(NOCC))*CELLS(id2)%out_data(ODUIO_EAT)
                    EAT_COUNT = EAT_COUNT + 1
                    EAT_INFO(cn2+1)   = EAT_INFO(cn2+1) + 1
                    EATEN_INFO(cn1+1) = EATEN_INFO(cn1+1) + 1
                    CELLS(id2)%E = CELLS(id2)%E + EAT_COEFF*CELLS(id1)%E
                    CELLS(id1)%E = (1.0d0 - EAT_COEFF)*CELLS(id1)%E
                end If
            else If(cn2<cn1) then
                If(CELLS(id1)%EAT_AF==1) then
                    EAT_COEFF = ECEOE*(dble(cn1 - cn2)/dble(NOCC))*CELLS(id1)%out_data(ODUIO_EAT)
                    EAT_COUNT = EAT_COUNT + 1
                    EAT_INFO(cn1+1)   = EAT_INFO(cn1+1) + 1
                    EATEN_INFO(cn2+1) = EATEN_INFO(cn2+1) + 1
                    CELLS(id1)%E = CELLS(id1)%E + EAT_COEFF*CELLS(id2)%E
                    CELLS(id2)%E = (1.0d0 - EAT_COEFF)*CELLS(id2)%E
                end If
            end If
                 
            If(CELLS(id1)%ALONE_F == CELLS(id2)%ALONE_F) then
                return
            else
                
                If(CELLS(id1)%ALONE_F==1) then
                    If(CELLS(id2)%FUSION_AF==1) then
                        FUSION_COUNT = FUSION_COUNT + 1
                        CELLS(id2)%book = trim(adjustl(CELLS(id1)%book))//trim(adjustl(CELLS(id2)%book))
                        CELLS(id1)%E = (1.0d0 - ECEOF)*CELLS(id1)%E
                    end If
                else If(CELLS(id2)%ALONE_F==1) then
                    If(CELLS(id1)%FUSION_AF==1) then
                        FUSION_COUNT = FUSION_COUNT + 1
                        CELLS(id1)%book = trim(adjustl(CELLS(id2)%book))//trim(adjustl(CELLS(id1)%book))
                        CELLS(id2)%E = (1.0d0 - ECEOF)*CELLS(id2)%E
                    end If
                 end If
                 
            end If
        end If
        
    end subroutine sub_cell_hit_cell

    subroutine update_info_trans_color(id1, id2)
        Integer :: id1, id2
        Double precision :: Imax
        Double precision :: ar, ag, ab

        ar = CELLS(id1)%IOLr*CELLS(id2)%ar
        ag = CELLS(id1)%IOLg*CELLS(id2)%ag
        ab = CELLS(id1)%IOLb*CELLS(id2)%ab

        CELLS(id2)%in_data(IDUIO_LR) = CELLS(id2)%in_data(IDUIO_LR) + ar
        CELLS(id2)%in_data(IDUIO_LG) = CELLS(id2)%in_data(IDUIO_LG) + ag
        CELLS(id2)%in_data(IDUIO_LB) = CELLS(id2)%in_data(IDUIO_LB) + ab

        CELLS(id2)%E = CELLS(id2)%E + ECEOL_L2E*(ECEOLR*ar + ECEOLG*ag + ECEOLB*ab)
        
        CELLS(id1)%IOLr = CELLS(id1)%IOLr - ar
        CELLS(id1)%IOLg = CELLS(id1)%IOLg - ag
        CELLS(id1)%IOLb = CELLS(id1)%IOLb - ab

        CELLS(id1)%E = CELLS(id1)%IOLr + CELLS(id1)%IOLg + CELLS(id1)%IOLb

        Imax = CELLS(id1)%IOLr
        If(Imax<CELLS(id1)%IOLg) then
            Imax = CELLS(id1)%IOLg
        end If
        If(Imax<CELLS(id1)%IOLb) then
            Imax = CELLS(id1)%IOLb
        end If
        If(Imax<1.0d-17) then
            CELLS(id1)%cr = 1.0d0
            CELLS(id1)%cg = 1.0d0
            CELLS(id1)%cb = 1.0d0
        else
            CELLS(id1)%cr = CELLS(id1)%IOLr/Imax
            CELLS(id1)%cg = CELLS(id1)%IOLg/Imax
            CELLS(id1)%cb = CELLS(id1)%IOLb/Imax
        end If 

    end subroutine update_info_trans_color

    subroutine cell_spring_cell()
        Integer :: i, c, id1, id2, n, m, bp, bpfb
        Double precision :: x, y, z, r, l, k, d, h
        Double precision :: fx, fy, fz
        Double precision :: free_ene

        n = -1
        m = -1
        c = 1

        Do c=1, CONN_CLL
            id1 = CONN_CL(c, 1, 1)
            n = CONN_CL(c, 1, 2)
            id2 = CONN_CL(c, 2, 1)
            m = CONN_CL(c, 2, 2)

            x = CELLS(id1)%x - CELLS(id2)%x
            y = CELLS(id1)%y - CELLS(id2)%y
            z = CELLS(id1)%z - CELLS(id2)%z
            r = sqrt(x**2 + y**2 + z**2)
            l = CELLS(id1)%LOCC(n) + CELLS(id2)%LOCC(m)
            If(SPRING_BREAK_C*l<r) then
                call delete_BACK_CONN_CL(id1, id2)
                cycle
            end If
            k = (CELLS(id1)%KOCC(n) + CELLS(id2)%KOCC(m))/2.0d0
            d = r - l
            If(CELLS(id1)%WAIT_FOR_DISCONNECT_F==1 .and. CELLS(id2)%WAIT_FOR_DISCONNECT_F==1) then
                DISCONNECT_COUNT = DISCONNECT_COUNT + 1
                call delete_BACK_CONN_CL(id1, id2)
                h = CELLS(id1)%r + CELLS(id2)%r
                free_ene = CONN_COST_C*INIT_K*h**2
                CELLS(id1)%E = CELLS(id1)%E + free_ene
                CELLS(id2)%E = CELLS(id2)%E + free_ene
                bp = index(CELLS(id1)%book, CELLS(id1)%bookmarker) + BOOKMARKER_LENGTH + 1
                bp = bp + nint(decode(CELLS(id1)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                bpfb = 0
                Do i=1, BOOKMARKER_LENGTH
                   CELLS(id1)%bookmarker(i:i) = CELLS(id1)%book((bp+bpfb):(bp+bpfb))
                   bpfb = bpfb + 2
                end Do
                bp = bp + bpfb - 2 + 1
                CELLS(id1)%bookmarker_advance = CELLS(id1)%book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))
                CELLS(id1)%WAIT_FOR_DISCONNECT_F = 0

                bp = index(CELLS(id2)%book, CELLS(id2)%bookmarker) + BOOKMARKER_LENGTH + 1
                bp = bp + nint(decode(CELLS(id2)%bookmarker_advance, BD_BOOKMARKER_ADVANCE_S))
                bpfb = 0
                Do i=1, BOOKMARKER_LENGTH
                   CELLS(id2)%bookmarker(i:i) = CELLS(id2)%book((bp+bpfb):(bp+bpfb))
                   bpfb = bpfb + 2
                end Do
                bp = bp + bpfb - 2 + 1
                CELLS(id2)%bookmarker_advance = CELLS(id2)%book(bp:(bp+BOOKMARKER_ADVANCE_LENGTH-1))
                CELLS(id2)%WAIT_FOR_DISCONNECT_F = 0
                
                cycle
            end If

            k = k*d

            fx = -k*(x/r)
            fy = -k*(y/r)
            fz = -k*(z/r)

            CELLS(id1)%fx = CELLS(id1)%fx + fx
            CELLS(id1)%fy = CELLS(id1)%fy + fy
            CELLS(id1)%fz = CELLS(id1)%fz + fz

            CELLS(id2)%fx = CELLS(id2)%fx - fx
            CELLS(id2)%fy = CELLS(id2)%fy - fy
            CELLS(id2)%fz = CELLS(id2)%fz - fz

        end Do

    end subroutine cell_spring_cell

    subroutine cell_mechanics()
        Integer :: id, CALC_CLn
        !$omp parallel
        !$omp do private(id, CALC_CLn)
        Do CALC_CLn=1, CALC_CLL
            id = CALC_CL(CALC_CLn)
            If(CELLS(id)%MECHANICS_AF==1) then
                call sub_cell_mechanics(id)
            end If
        end Do
        !$omp end do
        !$omp end parallel
    end subroutine cell_mechanics

    subroutine sub_cell_mechanics(id)
        Integer :: id

        CELLS(id)%fx = CELLS(id)%fx - AIR_RESISTANCE_K*CELLS(id)%vx
        CELLS(id)%fy = CELLS(id)%fy - AIR_RESISTANCE_K*CELLS(id)%vy
        CELLS(id)%fz = CELLS(id)%fz - AIR_RESISTANCE_K*CELLS(id)%vz

        CELLS(id)%fy = CELLS(id)%fy - CELLS(id)%m*WORLD_G

    end subroutine sub_cell_mechanics

    subroutine update_cell()
        Integer :: i, j, n, id, CALC_CLn, cn
        Integer :: fp=12
        Integer :: count, count_it 
        Integer(8) :: rem 
        Integer, allocatable :: fragment_count(:)
        Double precision :: ax, ay, az 
        Double precision :: age
        Character(len=128) :: filename
        Character(len=FRAGMENT_MAX_LENGTH) :: fragment
        
        AGE_AVE = 0.0d0
        AGE_VAR = 0.0d0
        AGE_MIN = 2**63-1
        AGE_MAX = 0
        
        AGE_IT_AVE = 0.0d0
        AGE_IT_VAR = 0.0d0
        AGE_IT_MIN = 2**63-1
        AGE_IT_MAX = 0

        count = 0
        count_it = 0
        AGE_COUNT(:) = 0

        If(FIX_FLAG==1) then
           
           Do i=1, FIX_NUM
           
              id = FIX_LIST(i)

              CELLS(id)%vx = 0.0d0
              CELLS(id)%vy = 0.0d0
              CELLS(id)%vz = 0.0d0
            
              CELLS(id)%fx = 0.0d0
              CELLS(id)%fy = 0.0d0
              CELLS(id)%fz = 0.0d0

           end Do

        else If(FIX_FLAG==2) then

           Do i=1, FIX_NUM
           
              id = FIX_LIST(i)

              CELLS(id)%vx = 0.0d0
              CELLS(id)%vy = 0.0d0
              CELLS(id)%vz = 0.0d0
            
              CELLS(id)%fx = 0.0d0
              CELLS(id)%fy = 0.0d0
              CELLS(id)%fz = 0.0d0

              CELLS(id)%E = FIX_ENERGY(i)
              
           end Do
           
        end If

        call set_every_step_cost_list()

        Do CALC_CLn=1, CALC_CLL
            id = CALC_CL(CALC_CLn)

            age = dble(CELLS(id)%age)

            If(CELLS(id)%INFO_TRANS_F==0) then
               If(CELLS(id)%ALONE_F==1) then
                  ALONE_COUNT = ALONE_COUNT + 1
               end If
               If(id/=SUN_ID) then
                  cn = sum(CELLS(id)%CCF(:))
                  !CELLS(id)%E = ( 1.0d0 - EVERY_STEP_COST_E*(EVERY_STEP_COST_A**(cn-NOCC)))*CELLS(id)%E
                  CELLS(id)%E = (1.0d0 - EVERY_STEP_COST_LIST(cn+1))*CELLS(id)%E
                  
                  TOT_ENERGY = TOT_ENERGY + CELLS(id)%E
                  CENTER_OF_GRAV_X = CENTER_OF_GRAV_X + CELLS(id)%m*CELLS(id)%x
                  CENTER_OF_GRAV_Y = CENTER_OF_GRAV_Y + CELLS(id)%m*CELLS(id)%y
                  CENTER_OF_GRAV_Z = CENTER_OF_GRAV_Z + CELLS(id)%m*CELLS(id)%z
                  TOT_M            = TOT_M            + CELLS(id)%m
                  
                  rem = TRANS_INTERVAL_STEP + mod(CELLS(id)%age, TRANS_INTERVAL_STEP) - mod(WORLD_STEP, TRANS_INTERVAL_STEP)
                  rem = mod(rem, TRANS_INTERVAL_STEP) + 1
                  AGE_COUNT(rem) = AGE_COUNT(rem) + 1
                  AGE_VAR       = (dble(count)/dble(count+1))*AGE_VAR
                  AGE_VAR       = AGE_VAR + (dble(count)/dble((count + 1)**2))*((age-AGE_AVE)**2)
                  AGE_AVE          = (dble(count)*AGE_AVE + age)/dble(count + 1)

                  if(CELLS(id)%age<AGE_MIN) then
                     AGE_MIN = CELLS(id)%age
                  end if
                  if(AGE_MAX<CELLS(id)%age) then
                     AGE_MAX = CELLS(id)%age
                  end if
                  count = count + 1
               end If
            else
               AGE_IT_VAR          = (dble(count_it)/dble(count_it+1))*AGE_IT_VAR
               AGE_IT_VAR          = AGE_IT_VAR + (dble(count_it)/dble((count_it + 1)**2))*((age-AGE_IT_AVE)**2)
               AGE_IT_AVE          = (dble(count_it)*AGE_IT_AVE + age)/dble(count_it + 1)
               if(CELLS(id)%age<AGE_IT_MIN) then
                  AGE_IT_MIN = CELLS(id)%age
               end if
               if(AGE_IT_MAX<CELLS(id)%age) then
                  AGE_IT_MAX = CELLS(id)%age
               end if
               count_it = count_it + 1
            end If
            
            If(CELLS(id)%E<LIMIT_E) then
                If(CELLS(id)%INFO_TRANS_F==0) then
                   DEATHS_COUNT = DEATHS_COUNT + 1
                end If
                call free_cell(id)
                call delete_BACK_CALC_CL(id)
                call add_NEW_NOT_CALC_CL(id)
                cycle
            end If

            CELLS(id)%age = CELLS(id)%age + 1

            ax = CELLS(id)%fx/CELLS(id)%m
            ay = CELLS(id)%fy/CELLS(id)%m
            az = CELLS(id)%fz/CELLS(id)%m

            CELLS(id)%vx = CELLS(id)%vx + ax*WORLD_DT
            CELLS(id)%vy = CELLS(id)%vy + ay*WORLD_DT
            CELLS(id)%vz = CELLS(id)%vz + az*WORLD_DT
            
            CELLS(id)%x = CELLS(id)%x + CELLS(id)%vx*WORLD_DT 
            CELLS(id)%y = CELLS(id)%y + CELLS(id)%vy*WORLD_DT 
            CELLS(id)%z = CELLS(id)%z + CELLS(id)%vz*WORLD_DT 
            
            CELLS(id)%fx = 0.0d0
            CELLS(id)%fy = 0.0d0
            CELLS(id)%fz = 0.0d0

            CELLS(id)%ALONE_F = 1
        end Do

        CELLS(SUN_ID)%E = SUN_ENERGY        
        CELLS(SUN_ID)%x = SUN_AMP*dble(FIELD_SIZE_X)*sin(2.0d0*PI*(dble(WORLD_STEP)/SUN_CYCLE))

        If(FIX_FLAG==1) then
        else If(FIX_FLAG==2) then
           Do i=1, FIX_NUM
              id = FIX_LIST(i)
              CELLS(id)%E = FIX_ENERGY(i)
           end Do
        end If

        If(FRAGMENT_FLAG==1 .and. mod(WORLD_STEP-1, FRAGMENT_INTERVAL_STEP)==0) then
           allocate(fragment_count(FRAGMENT_NUM))
           fragment_count(:) = 0
           Do CALC_CLn=1, CALC_CLL
              id = CALC_CL(CALC_CLn)
              If(CELLS(id)%INFO_TRANS_F==0) then
                 Do i=1, FRAGMENT_NUM
                    fragment(:) = ""
                    Do j=1, FRAGMENT_MAX_LENGTH
                       n = FRAGMENT_LIST(i, j)
                       If(n==0) then
                          exit
                       end If
                       fragment(j:j) = CODES(n:n)
                    end Do
                    If(index(CELLS(id)%book, trim(adjustl(fragment)))/=0) then
                       fragment_count(i) = fragment_count(i) + 1
                    end If
                 end Do
              end If
           end Do
           Do i=1, FRAGMENT_NUM
              write (filename, '("fragment", i4.4, ".d")') i
              open(fp, file=trim(filename), status="old", position="append")
              write(fp, *) WORLD_STEP, fragment_count(i)
              close(fp)
           end Do
           deallocate(fragment_count)

        else If(FRAGMENT_FLAG==2 .and. mod(WORLD_STEP-1, FRAGMENT_INTERVAL_STEP)==0) then

           open(fp, file="fragment.d", status="old", position="append")
  
           write(fp, *) "WORLD_STEP=", WORLD_STEP 
           Do CALC_CLn=1, CALC_CLL
              id = CALC_CL(CALC_CLn)
              If(CELLS(id)%INFO_TRANS_F==0) then
                 Do i=1, FRAGMENT_NUM
                    fragment(:) = ""
                    Do j=1, FRAGMENT_MAX_LENGTH
                       n = FRAGMENT_LIST(i, j)
                       If(n==0) then
                          exit
                       end If
                       fragment(j:j) = CODES(n:n)
                    end Do
                    If(index(CELLS(id)%book, trim(adjustl(fragment)))/=0) then
                       write(fp, *) id, i, CELLS(id)%x, CELLS(id)%y, CELLS(id)%z, CELLS(id)%E
                    end If
                 end Do
              end If
           end Do
           
           close(fp)
             
        end If
        
        If(count==0) then
           AGE_MIN = 0
        end If
        
        If(count_it==0) then
           AGE_IT_MIN = 0
        end If
       
    end subroutine update_cell    
    
    subroutine get_world_step(n)  bind(c)
      Integer(c_int) :: n
      n = WORLD_STEP
    end subroutine get_world_step
    
    subroutine get_field_size(x, y, z)  bind(c)
        Integer(c_int) :: x, y, z
        x = FIELD_SIZE_X
        y = FIELD_SIZE_Y
        z = FIELD_SIZE_Z
    end subroutine get_field_size

    subroutine get_field_center(x, y, z)  bind(c)
        Integer(c_int) :: x, y, z
        x = FIELD_CENTER_X
        y = FIELD_CENTER_Y
        z = FIELD_CENTER_Z
    end subroutine get_field_center

    subroutine get_field(x, y, z, n)  bind(c)
        Integer(c_int) :: x, y, z, n
        n = FIELD((x+1), (y+1), (z+1))%wall
    end subroutine get_field

    subroutine get_NOC(n)  bind(c)
        Integer(c_int) :: n
        n = NUMBER_OF_CELL
    end subroutine get_NOC

    subroutine get_cell(ID, exist, x, y, z, r, cr, cg, cb)  bind(c)
        Integer(c_int) :: ID, exist
        Real(c_double) :: x, y, z, r, cr, cg, cb
        If(NUMBER_OF_CELL<(ID+1)) then
            return
        end If
        exist = CELLS(ID+1)%exist
        x = CELLS(ID+1)%x
        y = CELLS(ID+1)%y
        z = CELLS(ID+1)%z
        r = CELLS(ID+1)%r
        cr = CELLS(ID+1)%cr
        cg = CELLS(ID+1)%cg
        cb = CELLS(ID+1)%cb
    end subroutine get_cell

    subroutine get_cell_info(ID, x, y, z, r, cr, cg, cb, e, sum_ccf, itf, oe, age) bind(c)
        Integer(c_int) :: ID, sum_ccf, itf 
        Integer(c_int64_t) :: age
        Real(c_double) :: x, y, z, r, cr, cg, cb, e, oe
        If(ID<1 .or. NUMBER_OF_CELL<ID) then
           return
        end If
        x = CELLS(ID)%x
        y = CELLS(ID)%y
        z = CELLS(ID)%z
        r = CELLS(ID)%r
        cr = CELLS(ID)%cr
        cg = CELLS(ID)%cg
        cb = CELLS(ID)%cb
        e = CELLS(ID)%E
        sum_ccf = sum(CELLS(ID)%CCF(:))
        itf = CELLS(ID)%INFO_TRANS_F
        oe  = CELLS(ID)%out_data(ODUIO_EAT)
        age = CELLS(ID)%age
    end subroutine get_cell_info
    
    subroutine get_CALC_CLL(n)  bind(c)
        Integer(c_int) :: n
        n = CALC_CLL
    end subroutine get_CALC_CLL

    subroutine get_CALC_CL(CALC_CLn, ID, x, y, z, r, cr, cg, cb, e, sum_ccf, itf, oe, age) bind(c)
        Integer(c_int) :: CALC_CLn, ID, sum_ccf, itf 
        Integer(c_int64_t) :: age
        Real(c_double) :: x, y, z, r, cr, cg, cb, e, oe
        If(CALC_CLL<(CALC_CLn+1)) then
            return
        end If
        ID = CALC_CL(CALC_CLn+1)
        If(ID<1 .or. NUMBER_OF_CELL<ID) then
           return
        end If
        x = CELLS(ID)%x
        y = CELLS(ID)%y
        z = CELLS(ID)%z
        r = CELLS(ID)%r
        cr = CELLS(ID)%cr
        cg = CELLS(ID)%cg
        cb = CELLS(ID)%cb
        e = CELLS(ID)%E
        sum_ccf = sum(CELLS(ID)%CCF(:))
        itf = CELLS(ID)%INFO_TRANS_F
        oe  = CELLS(ID)%out_data(ODUIO_EAT)
        age = CELLS(ID)%age
    end subroutine get_CALC_CL

    subroutine get_cc_list_length(l)  bind(c)
        Integer(c_int) :: l
        l = CONN_CLL
    end subroutine get_cc_list_length

    subroutine get_cc_list(c, id1, id2)  bind(c)
        Integer(c_int) :: c, id1, id2
        If((CONN_CLL-1)<c) then
            id1 = -1
            id2 = -1
            c = CONN_CLL
            return
        end If
        id1 = CONN_CL((c+1), 1, 1) - 1
        id2 = CONN_CL((c+1), 2, 1) - 1
    end subroutine get_cc_list

    subroutine get_block_list_length(l)  bind(c)
        Integer(c_int) :: l
        l = block_list_length
    end subroutine get_block_list_length

    subroutine get_block_list(c, w, x, y, z, cr, cg, cb)  bind(c)
        Integer(c_int) :: c, w
        Real(c_double) :: x, y, z, cr, cg, cb
        w = block_list(c+1)%wall
        x = block_list(c+1)%x
        y = block_list(c+1)%y
        z = block_list(c+1)%z
        cr = block_list(c+1)%cr
        cg = block_list(c+1)%cg
        cb = block_list(c+1)%cb
    end subroutine get_block_list

    subroutine get_trans_interval_step(n)  bind(c)
      Integer(c_int) :: n
      n = TRANS_INTERVAL_STEP
    end subroutine get_trans_interval_step

    subroutine get_age_count(n, ret_age_count)  bind(c)
      Integer(c_int), intent(in) :: n
      Real(c_double), intent(out) :: ret_age_count(n)
      Integer(c_int) :: i
      do i=1, n
         ret_age_count(i) = dble(AGE_COUNT(i))
      end do
    end subroutine get_age_count

    subroutine get_nn_param(ret_noiu, ret_nohu, ret_noou)  bind(c)
      Integer(c_int) :: ret_noiu, ret_nohu, ret_noou
      ret_noiu = NOIU
      ret_nohu = NOHU
      ret_noou = NOOU
    end subroutine get_nn_param

    subroutine get_nn(id, input, h, output)  bind(c)
      Integer(c_int), intent(in) :: id
      Real(c_double), intent(out) :: input(NOIU)
      Real(c_double), intent(out) :: h(NOHU)
      Real(c_double), intent(out) :: output(NOOU)

      Integer(c_int) :: i
      Real(c_double) :: input_(NOIU+1)
      Real(c_double) :: h_(NOHU+1)
      
      input_(1) = 1.0d0
      input_(2:(NOIU+1)) = CELLS(id)%in_data(:)
      
      h_(1) = 1.0d0
      Do i=2, NOHU+1
         h_(i) = tanh(sum(input_(:)*CELLS(id)%w1((i-1),:)))
      end Do

      input(:)  = CELLS(id)%in_data(:)
      h(:)      = h_(2:(NOHU+1))
      output(:) = CELLS(id)%out_data(:)

    end subroutine get_nn

    subroutine get_stat(nocw, te)  bind(c)
      Integer(c_int) :: nocw
      Real(c_double) :: te
      nocw = NUMBER_OF_CELLS_WOIT
      !write(*, *) cc-noi
      te  = TOT_ENERGY_FOR_C
    end subroutine get_stat
    
    subroutine get_double_test(n)  bind(c)
        Integer(c_int) :: n
        Real(c_double) :: x(5)
        x(1) = 1.0d0
        x(2) = 2.0d0
        x(3) = 3.0d0
        x(4) = 4.0d0
        x(5) = 5.0d0
        write(*,*) x(n)
    end subroutine get_double_test

    subroutine get_mutation_rate(x, z, mr) bind(c)
      Real(c_double) :: x, z
      Real(c_double) :: mr

      call return_mutation_rate(x, z, mr)
            
    end subroutine get_mutation_rate

    subroutine get_mutation_param(mra, mc) bind(c)
      Real(c_double) :: mra, mc
      mra = MUTATION_RATE_AMP
      mc = MUTATION_COEFF4EXPANSION      
    end subroutine get_mutation_param

    subroutine set_mutation_param(mra, mc) bind(c)
      Real(c_double) :: mra, mc
      MUTATION_RATE_AMP = mra
      MUTATION_COEFF4EXPANSION = mc 
    end subroutine set_mutation_param

    subroutine set_out_interval_step(n) bind(c)
      Integer(c_int) :: n
      OUT_INTERVAL_STEP = n
    end subroutine set_out_interval_step
    
end module praparat_module

