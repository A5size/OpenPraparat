program main
    use math
    use Praparat_module
    Implicit none
    Character(len=128) :: filename
    Integer :: fp, i, out_cycle, in_cycle

    namelist/cycle/out_cycle
    namelist/cycle/in_cycle

    fp = 12

    open(fp, file='./input')
    read(fp, nml=cycle)
    close(fp)

    write(*, nml=cycle)
    
    call praparat_init()
    Do i=1, out_cycle
       call step(in_cycle)
       write (filename, '("cells", i0, ".dat")') i
       call write_cells_into_one(trim(adjustl(filename)))
    end Do

end program main
