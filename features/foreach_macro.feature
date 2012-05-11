Feature: Foreach Macros

  In order to abstract away different looping strategies we need to
  provide a looping macro.

  Scenario: basic foreach macro
    Given a foreach macro named "particles" with argument list (particle_set)
    And body
    """
    do <%= iter %>=1,<%= particle_set %>%Npart
    <%= body.indent 2 -%>
    end do

    """
    When I preprocess
    """
      ! leading comment
      foreach p in particles(P)
        ! do something to p
        p = 20
      end foreach

    """
    Then it should expand into
    """
      ! leading comment
      do p=1,P%Npart
        ! do something to p
        p = 20
      end do

    """

  Scenario: loop nesting
    Given a foreach macro named "outer" with argument list (a)
    And body
    """
    do <%= iter %>=1,<%= a %>%Npart
    <%= body.indent 2 -%>
    end do

    """
    And a foreach macro named "inner" with argument list (b)
    And body
    """
    do <%= iter %>=1,<%= b %>%Npart
    <%= body.indent 2 -%>
    end do

    """
    When I preprocess
    """
      foreach p in outer(X)
        foreach q in inner(Y)
          bla
        end foreach
      end foreach

    """
    Then it should expand into
    """
      do p=1,X%Npart
        do q=1,Y%Npart
          bla
        end do
      end do

    """

  Scenario: particle foreach macro
    Given a foreach macro named "particles" with argument list (pset)
    And body
    """
    modifier positions(x)
    modifier fields(*fields)
    modifier types(*types)
    % unless x == :required
    call P%get_xp(<%= "#{x}_#{iter}" %>,info)
    % end
    % fields.each do |f|
    call P%get_field(<%= f[1] %>,<%= "#{f[0]}_#{iter}" %>,info)
    %   body.transform! "#{f[0]}_#{iter}", "#{f[0]}_#{iter}($1,#{iter})"
    % end
    do <%= iter %>=1,<%= pset %>%Npart
    % fields.each do |f|
    %   body.transform! "#{f[0]}_#{iter}", "#{f[0]}_#{iter}(#{iter})"
    % end
    <%= body.indent 2 -%>
    end do

    """
    And a foreach macro named "neighbors" with argument list (pset)
    And body
    """
    do <%= iter %>=1,<%= particle_set %>%Npart
    <%= body.indent 2 -%>
    end do

    """
    When I preprocess
    """
      ! leading comment
      foreach p in particles(P) with fields(w=weight,dw=change)
        ! updating w
        w_p = w_p + dt*dw_p
      end foreach

    """
    Then it should expand into
    """
      ! leading comment
      call P%get_field(weight,w_p,info)
      call P%get_field(change,dw_p,info)
      do p=1,P%Npart
        ! updating w
        w_p(p) = w_p(p) + dt*dw_p(p)
      end do

    """

  Scenario: modifiers
    Given setting ppm.dim is 2
    And a foreach macro named "mesh" with argument list (m)
    And body
    """
    modifier fields(*fields)
    modifier indices(i,j,k=nil)
    % body_indent = conf.ppm.dim * 2 + 2
    patch_iterator = <%= m %>%subpatch%begin()
    do while (associated(patch_iterator))
    % fields.each_with_index do |f,ind|
      call patch_iterator%get_field(field_data<%= ind %>, <%= f %>, info)
    % end
      do <%= i %> = 1, patch_iterator%nnodes(1)
        do <%= j %> = 1, patch_iterator%nnodes(2)
    % if conf.ppm.dim == 3
          do <%= k %> = 1, patch_iterator%nnodes(3)
    % end
    <%= body.indent body_indent -%>
    % if conf.ppm.dim == 3
          end do
    % end
        end do
      end do
      patch_iterator => <%= m %>%subpatch%next()
    end do

    """
    When I preprocess
    """
    foreach node in mesh(M) with fields(f1) indices(i,j)
      node%f1(1) = cos(i*h(1)+j)
      node%f1(2) = sin(i*h(1)+j)
      node%f1(3) = cos(i*h(1)+j)**2
    end foreach

    """
    Then it should expand into
    """
    patch_iterator = M%subpatch%begin()
    do while (associated(patch_iterator))
      call patch_iterator%get_field(field_data0, f1, info)
      do i = 1, patch_iterator%nnodes(1)
        do j = 1, patch_iterator%nnodes(2)
          node%f1(1) = cos(i*h(1)+j)
          node%f1(2) = sin(i*h(1)+j)
          node%f1(3) = cos(i*h(1)+j)**2
        end do
      end do
      patch_iterator => M%subpatch%next()
    end do

    """

  Scenario: identifier transform
    Given a foreach macro named "mesh" with argument list (m)
    And body
    """
    modifier indices(i=i,j=j,k=k)
    modifier fields(*fs)
    patch_iterator = <%= m %>%subpatch%begin()
    do while (associated(patch_iterator))
    % fs.each do |f|
      call patch_iterator%get_field(<%= f %>_data, <%= f %>, info)
    %   body.transform! "#{f}_#{iter}", "#{f}_data(#{i},#{j},$1)"
    % end
      do <%= i %> = 1, patch_iterator%nnodes(1)
        do <%= j %> = 1, patch_iterator%nnodes(2)
    <%= body.indent 6 -%>
        end do
      end do
      patch_iterator => <%= m %>%subpatch%next()
    end do

    """
    When I preprocess
    """
    foreach n in mesh(M) with fields(f1,f2,f3)
      f1_n(1) = cos(i*h(1)+j)
      f2_n(2) = sin(i*h(1)+j)
      f3_n(3) = cos(i*h(1)+j)**2
    end foreach

    """
    Then it should expand into
    """
    patch_iterator = M%subpatch%begin()
    do while (associated(patch_iterator))
      call patch_iterator%get_field(f1_data, f1, info)
      call patch_iterator%get_field(f2_data, f2, info)
      call patch_iterator%get_field(f3_data, f3, info)
      do i = 1, patch_iterator%nnodes(1)
        do j = 1, patch_iterator%nnodes(2)
          f1_data(i,j,1) = cos(i*h(1)+j)
          f2_data(i,j,2) = sin(i*h(1)+j)
          f3_data(i,j,3) = cos(i*h(1)+j)**2
        end do
      end do
      patch_iterator => M%subpatch%next()
    end do

    """

  Scenario: multiple bodies
    Given a foreach macro named "mesh" with argument list (m)
    And body
    """
    i = 1
    do j = 1, patch_iterator%nnodes(2)
    <%= bodies.top.indent 2 -%>
    end do

    do i = 2, patch_iterator%nnodes(1) - 1
      do j = 1, patch_iterator%nnodes(2)
    <%= bodies.rest.indent 4 -%>
      end do
    end do

    i = patch_iterator%nnodes(1)
    do j = 1, patch_iterator%nnodes(2)
    <%= bodies.bottom.indent 2 -%>
    end do

    """
    When I preprocess
    """
    foreach n in mesh(M)
      for top
        x = f(i+1,j)
      for bottom
        x = f(i-1,j)
      for rest
        x = f(i-1,j) + f(i+1,j)
    end foreach

    """
    Then it should expand into
    """
    i = 1
    do j = 1, patch_iterator%nnodes(2)
      x = f(i+1,j)
    end do

    do i = 2, patch_iterator%nnodes(1) - 1
      do j = 1, patch_iterator%nnodes(2)
        x = f(i-1,j) + f(i+1,j)
      end do
    end do

    i = patch_iterator%nnodes(1)
    do j = 1, patch_iterator%nnodes(2)
      x = f(i-1,j)
    end do

    """
    
