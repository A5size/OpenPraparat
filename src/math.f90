module math
    implicit none

    Double precision, save :: mac_e = 2.0d-53
    Double precision, save :: PI = 2.0d0*acos(0.0d0)
    Complex(kind(0d0)), save :: ei = (0.0d0, 1.0d0)

    interface det_rec
        module procedure d_det, c_det
    end interface

    interface det
        module procedure D_CAL_DET, C_CAL_DET
    end interface

    interface swap
        module procedure i_swap, d_swap
    end interface

    interface sort
        module procedure i_sort, d_sort
    end interface

    interface average
        module procedure i_average, d_average, c_average
    end interface

    interface var
        module procedure i_var, d_var, c_var
    end interface

    interface std
        module procedure i_std, d_std, c_std
    end interface

contains

    function prime(k, nc) result(p)
        Integer, intent(in) :: k
        Integer, intent(in), optional :: nc
        Integer :: nmcycle, count
        Integer(8) :: p
        Integer(8) :: i, N, point
        Integer(1), allocatable :: p_list(:)
        Double precision :: x

        If(k<10) then
            If(k==1) then
                p = 2
            else If(k==2) then
                p = 3
            else
                p = (6*k - 9 - (-1)**k)/2
            end If
            return
        end If

        x = dble(k)

        If(present(nc)) then
            nmcycle = nc
        else
            nmcycle = 3
        end If

        Do i=1, nmcycle
            x = x - ((x-dble(k)*log(x))*log(x))/(log(x)-1.0d0)
        end Do

        N = nint(x)

        If(N<0) then
            write(*,*) 'N=', N
            write(*,*) 'error'
            p = 0
            return
        end If

        If(N<2) then
            N = 2
        end If

        Do
            allocate(p_list(N))
            p_list(:) = 0 
            p = 2
            count = 1

            Do
                If(count==k .or. p==N) then
                    exit
                end If

                point = p
                Do
                    p_list(point) = 1
                    point = point + p
                    If(N<point) then
                        exit
                    end If
                end Do

                Do i=(p+1), N
                    !write(*,*) i, p_list(i)
                    If(p_list(i)==0) then
                        p = i
                        count = count + 1
                        exit
                    end If
                end Do

                If(N<i) then
                    exit
                end If

            end Do

            If(count==k) then
                exit
            else
                write(*,*) 'Re-strat'
                deallocate(p_list)
                N = 2*N
            end If

        end Do

    end function prime

    function prime_bit(k, nc) result(p)
        Integer, intent(in) :: k
        Integer, intent(in), optional :: nc
        Integer :: nmcycle, count, rs_count, max_rs = 5
        Integer(8) :: p
        Integer(8) :: i, N, plN, point, point_b, point_inb
        Integer, allocatable :: p_list(:)
        Double precision :: x

        If(k<10) then
            If(k==1) then
                p = 2
            else If(k==2) then
                p = 3
            else
                p = (6*k - 9 - (-1)**k)/2
            end If
            return
        end If

        rs_count = 1
        x = dble(k)

        If(present(nc)) then
            nmcycle = nc
        else
            nmcycle = 3
        end If

        Do i=1, nmcycle
            x = x - ((x-dble(k)*log(x))*log(x))/(log(x)-1.0d0)
        end Do

        plN = int(x/32.0d0 + 1.0d0)

        If(plN<0) then
            write(*,*) 'plN=', plN
            write(*,*) 'error'
            p = 0
            return
        end If

        If(plN<1) then
            plN = 1
        end If

        Do
            allocate(p_list(plN))
            N = plN*32 - 1
            p_list(:) = 0 
            p = 2
            count = 1

            Do
                If(count==k .or. p==N) then
                    exit
                end If

                point = p
                Do
                    !p_list(point) = 1
                    point_b = point/32 + 1
                    point_inb = mod(point, 32)
                    p_list(point_b) = ibset(p_list(point_b), point_inb)
                    point = point + p
                    If(N<point) then
                        exit
                    end If
                end Do

                Do i=(p+1), N
                    !write(*,*) i, p_list(i)
                    point_b = i/32 + 1
                    point_inb = mod(i, 32)
                    If(.not.btest(p_list(point_b), point_inb)) then
                        p = i
                        count = count + 1
                        exit
                    end If
                end Do

                If(N<i) then
                    exit
                end If

            end Do

            If(count==k) then
                exit
            else
                If(max_rs<rs_count) then
                    write(*,*) 'Re-strat error', max_rs
                    exit
                end If
                write(*,*) 'Re-strat'
                deallocate(p_list)
                plN = 2*plN
                rs_count = rs_count + 1
            end If

        end Do

    end function prime_bit

    recursive function d_det(A) result(D)
        Integer :: i, n
        Double precision, intent(in) :: A(:,:)
        Double precision, allocatable :: b(:,:)
        Double precision :: D

        n = size(A,1)
        allocate(b(n-1,n-1))

        If(n>1) then
            D = 0.0d0
            Do i=1, n
                If(i==1) then
                    b(1:n-1, 1:n-1) = A(2:n, 2:n)
                else
                    b(1:i-1, 1:n-1) = A(1:i-1, 2:n)
                    b(i:n-1, 1:n-1) = A(i+1:n, 2:n)
                end If
                D = D + ((-1.0d0)**(i+1))*A(i,1)*d_det(b)
            end Do
        else
            D = A(1,1)
        end If

    end function d_det

    recursive function c_det(A) result(D)
        Integer :: i, n
        Complex(kind(0d0)), intent(in) :: A(:,:)
        Complex(kind(0d0)), allocatable :: b(:,:)
        Complex(kind(0d0)) :: D

        n = size(A,1)
        allocate(b(n-1,n-1))

        If(n>1) then
            D = 0.0d0 + ei*0.0d0
            Do i=1, n
                If(i==1) then
                    b(1:n-1, 1:n-1) = A(2:n, 2:n)
                else
                    b(1:i-1, 1:n-1) = A(1:i-1, 2:n)
                    b(i:n-1, 1:n-1) = A(i+1:n, 2:n)
                end If
                D = D + ((-1.0d0)**(i+1))*A(i,1)*c_det(b)
            end Do
        else
            D = A(1,1)
        end If

    end function c_det

    function D_CAL_DET(in_A) result(D)
        Integer :: n
        Integer :: i, j
        Double precision, intent(in) :: in_A(:,:)
        Double precision, allocatable :: A(:,:)
        Double precision, allocatable :: row(:)
        Double precision :: D
        Double precision :: c
        Double precision :: k

        n = size(in_A, 1)
        allocate(A(n,n))
        allocate(row(n))

        A(:,:) = in_A(:,:)
        D = 1.0d0
        c = 1.0d0

        Do j=1, n

            If(abs(A(j,j))<mac_e) then
                row(:) = A(j,:)
                Do i=(j+1), n
                    If(abs(A(i,j))>mac_e) then
                        A(j,:) = A(i,:)
                        A(i,:) = row(:)
                        c = -1.0d0*c
                    end If
                end Do
            end If

            Do i=(j+1), n
                If(abs(A(i,j))<mac_e) then
                    cycle
                end If
                k = -A(i,j)/A(j,j)
                A(i,:) = A(i,:) + k*A(j,:)
            end Do

        end Do

        Do i=1, n
            D = D*A(i,i)
        end Do

        deallocate(A)
        deallocate(row)

    end function D_CAL_DET

    function C_CAL_DET(in_A) result(D)
        Integer :: n
        Integer :: i, j
        Complex(kind(0d0)), intent(in) :: in_A(:,:)
        Complex(kind(0d0)), allocatable :: A(:,:)
        Complex(kind(0d0)), allocatable :: row(:)
        Complex(kind(0d0)) :: D
        Complex(kind(0d0)) :: c
        Complex(kind(0d0)) :: k

        n = size(in_A, 1)
        allocate(A(n,n))
        allocate(row(n))

        A(:,:) = in_A(:,:)
        D = 1.0d0 + ei*0.0d0
        c = 1.0d0 + ei*0.0d0

        Do j=1, n

            If(abs(A(j,j))<mac_e) then
                row(:) = A(j,:)
                Do i=(j+1), n
                    If(abs(A(i,j))>mac_e) then
                        A(j,:) = A(i,:)
                        A(i,:) = row(:)
                        c = -1.0d0*c
                    end If
                end Do
            end If

            Do i=(j+1), n
                If(abs(A(i,j))<mac_e) then
                    cycle
                end If
                k = -A(i,j)/A(j,j)
                A(i,:) = A(i,:) + k*A(j,:)
            end Do

        end Do

        Do i=1, n
            D = D*A(i,i)
        end Do

        deallocate(A)
        deallocate(row)

    end function C_CAL_DET

    function i_average(A) result(f)
        Integer, intent(in) :: A(:)
        Integer :: n
        Double precision :: f
        n = size(A,1)
        f = dble(sum(A))/dble(n)
    end function i_average

    function d_average(A) result(f)
        Double precision, intent(in) :: A(:)
        Integer :: n
        Double precision :: f
        n = size(A,1)
        f = sum(A)/dble(n)
    end function d_average

    function c_average(A) result(f)
        Complex(kind(0d0)), intent(in) :: A(:)
        Integer :: n
        Complex(kind(0d0)) :: f
        n = size(A,1)
        f = sum(A)/dble(n)
    end function c_average

    function i_var(A, ddof) result(f)
        Integer, intent(in) :: A(:)
        Integer, intent(in), optional :: ddof
        Integer :: n
        Double precision :: ave
        Double precision :: f

        n = size(A,1)
        ave = i_average(A)
        f = sum((dble(A(:))-ave)**2)

        If(present(ddof)) then
            If(ddof==1) then
                f = f/dble(n-1)
            else
                f = f/dble(n)
            end If
        else
            f = f/dble(n)
        end If

    end function i_var

    function d_var(A, ddof) result(f)
        Double precision, intent(in) :: A(:)
        Integer, intent(in), optional :: ddof
        Integer :: n
        Double precision :: ave
        Double precision :: f

        n = size(A,1)
        ave = d_average(A)
        f = sum((A(:)-ave)**2)

        If(present(ddof)) then
            If(ddof==1) then
                f = f/dble(n-1)
            else
                f = f/dble(n)
            end If
        else
            f = f/dble(n)
        end If
        
    end function d_var

    function c_var(A, ddof) result(f)
        Complex(kind(0d0)), intent(in) :: A(:)
        Integer, intent(in), optional :: ddof
        Integer :: n
        Complex(kind(0d0)) :: ave
        Complex(kind(0d0)) :: f

        n = size(A,1)
        ave = c_average(A)
        f = sum((A(:)-ave)**2)

        If(present(ddof)) then
            If(ddof==1) then
                f = f/dble(n-1)
            else
                f = f/dble(n)
            end If
        else
            f = f/dble(n)
        end If
        
    end function c_var

    function i_std(A, ddof) result(f)
        Integer, intent(in) :: A(:)
        Integer, intent(in), optional :: ddof
        Double precision :: f

        f = sqrt(i_var(A, ddof))

    end function i_std

    function d_std(A, ddof) result(f)
        Double precision, intent(in) :: A(:)
        Integer, intent(in), optional :: ddof
        Double precision :: f

        f = sqrt(d_var(A, ddof))

    end function d_std

    function c_std(A, ddof) result(f)
        Complex(kind(0d0)), intent(in) :: A(:)
        Integer, intent(in), optional :: ddof
        Complex(kind(0d0)) :: f

        f = sqrt(c_var(A, ddof))

    end function c_std

    subroutine i_swap(v, i, j)
        Integer :: v(:), n, i, j
        Integer :: b

        n = size(v, 1)

        If(i>j) then
            If(j<1 .or. n<i) then
                write(*,*) 'error'
            else
                b = v(i)
                v(i) = v(j)
                v(j) = b
            end If
        else
            If(i<1 .or. n<j) then
                write(*,*) 'error'
            else
                b = v(i)
                v(i) = v(j)
                v(j) = b
            end If
        end If

    end subroutine i_swap

    subroutine d_swap(v, i, j)
        Integer :: n, i, j
        Double precision :: v(:)
        Double precision :: b

        n = size(v, 1)

        If(i>j) then
            If(j<1 .or. n<i) then
                write(*,*) 'error'
            else
                b = v(i)
                v(i) = v(j)
                v(j) = b
            end If
        else
            If(i<1 .or. n<j) then
                write(*,*) 'error'
            else
                b = v(i)
                v(i) = v(j)
                v(j) = b
            end If
        end If

    end subroutine d_swap

    subroutine c_swap(v, i, j)
        Integer :: n, i, j
        Complex(kind(0d0)) :: v(:)
        Complex(kind(0d0)) :: b

        n = size(v, 1)

        If(i>j) then
            If(j<1 .or. n<i) then
                write(*,*) 'error'
            else
                b = v(i)
                v(i) = v(j)
                v(j) = b
            end If
        else
            If(i<1 .or. n<j) then
                write(*,*) 'error'
            else
                b = v(i)
                v(i) = v(j)
                v(j) = b
            end If
        end If

    end subroutine c_swap

    recursive subroutine i_sort(A)
        Integer :: n, p, i, j, s, e
        Integer :: A(:)

        n = size(A, 1)

        If(n==1) then
            return
        else If(n==2) then
            If(A(1)>A(2)) then
                call swap(A,1,2)
                return
            else
                return
            end If
        else
            If(A(1)>A(2)) then
                p = A(1)
            else
                p = A(2)
            end If
        end If

        s = 0
        e = n

        Do

            Do i=(s+1), n
                If(A(i)>=p) then
                    s = i
                    exit
                end If
            end Do

            Do j=(n-e+1), n
                If(A((n-j+1))<p) then
                    e = n-j+1
                    exit
                end If
            end Do

            If(j>n) then
                s = 2
                e = 1
            end If

            If(s>=e) then
                call i_sort(A(1:(s-1)))
                call i_sort(A(s:n))
                exit
            else
                call swap(A, s, e)
            end If

        end Do

    end subroutine i_sort

    recursive subroutine d_sort(A)
        Integer :: n, i, j, s, e
        Double precision :: p
        Double precision :: A(:)

        n = size(A, 1)

        If(n==1) then
            return
        else If(n==2) then
            If(A(1)>A(2)) then
                call swap(A,1,2)
                return
            else
                return
            end If
        else
            If(A(1)>A(2)) then
                p = A(1)
            else
                p = A(2)
            end If
        end If

        s = 0
        e = n

        Do

            Do i=(s+1), n
                If(A(i)>=p) then
                    s = i
                    exit
                end If
            end Do

            Do j=(n-e+1), n
                If(A((n-j+1))<p) then
                    e = n-j+1
                    exit
                end If
            end Do

            If(j>n) then
                s = 2
                e = 1
            end If

            If(s>=e) then
                call d_sort(A(1:(s-1)))
                call d_sort(A(s:n))
                exit
            else
                call swap(A, s, e)
            end If

        end Do

    end subroutine d_sort

    subroutine rotation_x(x, y, z, rad)
        Double precision :: x, y, z, rad
        Double precision :: tmp_x, tmp_y, tmp_z

        tmp_x = x
        tmp_y = y
        tmp_z = z

        y = tmp_y*cos(rad) - tmp_z*sin(rad)
        z = tmp_y*sin(rad) + tmp_z*cos(rad)

    end subroutine rotation_x

    subroutine rotation_y(x, y, z, rad)
        Double precision :: x, y, z, rad
        Double precision :: tmp_x, tmp_y, tmp_z

        tmp_x = x
        tmp_y = y
        tmp_z = z

        x = tmp_x*cos(rad) + tmp_z*sin(rad)
        z = -tmp_x*sin(rad) + tmp_z*cos(rad)
    end subroutine rotation_y

    subroutine rotation_z(x, y, z, rad)
        Double precision :: x, y, z, rad
        Double precision :: tmp_x, tmp_y, tmp_z

        tmp_x = x
        tmp_y = y
        tmp_z = z

        x = tmp_x*cos(rad) - tmp_y*sin(rad)
        y = tmp_x*sin(rad) + tmp_y*cos(rad)
    end subroutine rotation_z

    subroutine timer(flag, time, kind)
        character(len=*) :: flag
        character(len=*), optional :: kind
        Integer, save :: t1
        Integer :: t2, t_rate, t_max, diff
        Double precision, save :: ct1
        Double precision :: ct2
        Double precision :: time

        If(present(kind)) then
            If(kind=='cpu' .or. kind=='c')then
                If(flag=='start' .or. flag=='s') then
                    call cpu_time(ct1)
                    time = 0.0d0
                    return
                else If(flag=='end' .or. flag=='e') then
                    call cpu_time(ct2)
                    time = ct2 - ct1
                    return
                end If
            end If
        end If

        If(flag=='start' .or. flag=='s') then
            call system_clock(t1)   ! 開始時を記録
            time = 0.0d0
            return
        else If(flag=='end' .or. flag=='e') then
            call system_clock(t2, t_rate, t_max)   ! 終了時を記録
            If ( t2 < t1 ) then
                diff = (t_max - t1) + t2 + 1
            else
                diff = t2 - t1
            end If
            time = dble(diff)/dble(t_rate)
        end If

    end subroutine timer

    subroutine cpu_timer(flag, ts, te, td)
        character(len=*) :: flag
        Double precision :: ts, te, td

        If(flag=='start' .or. flag=='s') then
           call cpu_time(ts)
           te = -1.0d0
           td = -1.0d0
           return
        else If(flag=='end' .or. flag=='e') then
           call cpu_time(te)
           td = te - ts
           return
        end If

    end subroutine cpu_timer

end module math
