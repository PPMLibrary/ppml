Feature: Foreach Macros

  In order to abstract away different looping strategies we need to
  provide a looping macro.

  Scenario: basic foreach macro
    Given a foreach macro named "parts" with argument list (particle_set)
    And body
    """
    do <%= iter %>=1,<%= particle_set %>%Npart
    <%= body.indent 2 -%>
    ENDDO

    """
    When I preprocess
    """
      ! leading comment
      foreach p in parts(P)
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
      ENDDO

    """

  Scenario: loop nesting
    Given a foreach macro named "outer" with argument list (a)
    And body
    """
    do <%= iter %>=1,<%= a %>%Npart
    <%= body.indent 2 -%>
    ENDDO

    """
    And a foreach macro named "inner" with argument list (b)
    And body
    """
    do <%= iter %>=1,<%= b %>%Npart
    <%= body.indent 2 -%>
    ENDDO

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
        ENDDO
      ENDDO

    """

  Scenario: particle neighlist foreach macro
    Given setting predictable_mangle_prefix is on
    When I preprocess
    """
      ! leading comment
      foreach p in particles(P) with sca_fields(w=weight,dw=change)
        foreach q in neighbors(p,nlist) with sca_fields(w=weight,dw=change)
          ! updating w
          dw_p = w_p*w_q
        end foreach
      end foreach

    """
    Then it should expand into
    """
      ! leading comment
      call P%get(weight,mangled_w_wp,info)
      IF (info.NE.0) THEN
        info = ppm_error_error
        CALL ppm_error(ppm_err_sub_failed, &
          "getting field weight for P",&
          caller, 5 , info)
        GOTO 9999
      END IF
      call P%get(change,mangled_dw_wp,info)
      IF (info.NE.0) THEN
        info = ppm_error_error
        CALL ppm_error(ppm_err_sub_failed, &
          "getting field change for P",&
          caller, 5 , info)
        GOTO 9999
      END IF
      do mangled_p=1,P%Npart
        do mangled_nvq=1,nlist%nvlist(mangled_p)
          mangled_q = nlist%vlist(mangled_nvq,mangled_p)
          ! updating w
          mangled_dw_wp(mangled_p) = mangled_w_wp(mangled_p)*mangled_w_wp(mangled_q)
        ENDDO
      ENDDO

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
    DO WHILE (ASSOCIATED(patch_iterator))
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
          ENDDO
    % end
        ENDDO
      ENDDO
      patch_iterator => <%= m %>%subpatch%next()
    ENDDO

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
    DO WHILE (ASSOCIATED(patch_iterator))
      call patch_iterator%get_field(field_data0, f1, info)
      do i = 1, patch_iterator%nnodes(1)
        do j = 1, patch_iterator%nnodes(2)
          node%f1(1) = cos(i*h(1)+j)
          node%f1(2) = sin(i*h(1)+j)
          node%f1(3) = cos(i*h(1)+j)**2
        ENDDO
      ENDDO
      patch_iterator => M%subpatch%next()
    ENDDO

    """

  Scenario: identifier transform
    Given a foreach macro named "mesh" with argument list (m)
    And body
    """
    modifier indices(i=i,j=j,k=k)
    modifier fields(*fs)
    patch_iterator = <%= m %>%subpatch%begin()
    DO WHILE (ASSOCIATED(patch_iterator))
    % fs.each do |f|
      call patch_iterator%get_field(<%= f %>_data, <%= f %>, info)
    %   body.transform! "#{f}_#{iter}", "#{f}_data(#{i},#{j},$1)"
    % end
      do <%= i %> = 1, patch_iterator%nnodes(1)
        do <%= j %> = 1, patch_iterator%nnodes(2)
    <%= body.indent 6 -%>
        ENDDO
      ENDDO
      patch_iterator => <%= m %>%subpatch%next()
    ENDDO

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
    DO WHILE (ASSOCIATED(patch_iterator))
      call patch_iterator%get_field(f1_data, f1, info)
      call patch_iterator%get_field(f2_data, f2, info)
      call patch_iterator%get_field(f3_data, f3, info)
      do i = 1, patch_iterator%nnodes(1)
        do j = 1, patch_iterator%nnodes(2)
          f1_data(i,j,1) = cos(i*h(1)+j)
          f2_data(i,j,2) = sin(i*h(1)+j)
          f3_data(i,j,3) = cos(i*h(1)+j)**2
        ENDDO
      ENDDO
      patch_iterator => M%subpatch%next()
    ENDDO

    """
  Scenario: injecting offsets
    Given a foreach macro named "mesh" with argument list (m)
    And body
    """
    modifier indices(i=i,j=j,k=k)
    modifier fields(*fs)
    patch_iterator = <%= m %>%subpatch%begin()
    DO WHILE (ASSOCIATED(patch_iterator))
    % fs.each do |f|
      call patch_iterator%get_field(<%= f %>_data, <%= f %>, info)
    %   body.transform! "#{f}_#{iter}", "#{f}_data(#{i} #1,#{j} #2,$1)"
    % end
      do <%= i %> = 1, patch_iterator%nnodes(1)
        do <%= j %> = 1, patch_iterator%nnodes(2)
    <%= body.indent 6 -%>
        ENDDO
      ENDDO
      patch_iterator => <%= m %>%subpatch%next()
    ENDDO

    """
    When I preprocess
    """
    foreach n in mesh(M) with fields(f1,f2,f3)
      f1_n(1) = f2_n[+1,](1) + f2_n[,-1](1)
    end foreach

    """
    Then it should expand into
    """
    patch_iterator = M%subpatch%begin()
    DO WHILE (ASSOCIATED(patch_iterator))
      call patch_iterator%get_field(f1_data, f1, info)
      call patch_iterator%get_field(f2_data, f2, info)
      call patch_iterator%get_field(f3_data, f3, info)
      do i = 1, patch_iterator%nnodes(1)
        do j = 1, patch_iterator%nnodes(2)
          f1_data(i ,j ,1) = f2_data(i +1,j ,1) + f2_data(i ,j -1,1)
        ENDDO
      ENDDO
      patch_iterator => M%subpatch%next()
    ENDDO

    """

  Scenario: multiple bodies
    Given a foreach macro named "mesh" with argument list (m)
    And body
    """
    modifier indices(i=i,j=j)
    <%= i %> = 1
    do <%= j %> = 1, patch_iterator%nnodes(2)
    <%= bodies.top.indent 2 -%>
    ENDDO

    do <%= i %> = 2, patch_iterator%nnodes(1) - 1
      do <%= j %> = 1, patch_iterator%nnodes(2)
    <%= bodies.rest.indent 4 -%>
      ENDDO
    ENDDO

    <%= i %> = patch_iterator%nnodes(1)
    do <%= j %> = 1, patch_iterator%nnodes(2)
    <%= bodies.bottom.indent 2 -%>
    ENDDO

    """
    When I preprocess
    """
    foreach n in mesh(M) with indices(i,j)
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
    ENDDO

    do i = 2, patch_iterator%nnodes(1) - 1
      do j = 1, patch_iterator%nnodes(2)
        x = f(i-1,j) + f(i+1,j)
      ENDDO
    ENDDO

    i = patch_iterator%nnodes(1)
    do j = 1, patch_iterator%nnodes(2)
      x = f(i-1,j)
    ENDDO

    """
    
  Scenario: complete example
    Given a foreach macro named "mesh" with argument list (m)
    And body
    """
    modifier fields(*fs)
    % fs.each do |f|
    call patch_iterator%get_field(<%= f %>_data, <%= f %>, info)
    % end
    % ft = CG::Transform.new "#{fs[0]}_#{iter}", "#{fs[0]}_data(i,j,$1)"
    % gt = CG::Transform.new "#{fs[1]}_#{iter}", "#{fs[1]}_data"
    % ft.transform! bodies.top
    % ft.transform! bodies.bottom
    % ft.transform! bodies.rest
    % gt.transform! bodies.top
    % gt.transform! bodies.bottom
    % gt.transform! bodies.rest

    i = 1
    do j = 1, patch_iterator%nnodes(2)
    <%= bodies.top.indent 2 -%>
    ENDDO

    do i = 2, patch_iterator%nnodes(1) - 1
      do j = 1, patch_iterator%nnodes(2)
    <%= bodies.rest.indent 4 -%>
      ENDDO
    ENDDO

    i = patch_iterator%nnodes(1)
    do j = 1, patch_iterator%nnodes(2)
    <%= bodies.bottom.indent 2 -%>
    ENDDO

    """
    When I preprocess
    """
    foreach n in mesh(M) with fields(f,g) indices(i,j)
      for top
        f_n(1) = g_n(i+1,j)
      for bottom
        f_n(1) = g_n(i-1,j)
        f_n(2) = g_n(i,j)
        f_n(3) = g_n(i+1,j)
      for rest
        f_n(1) = g_n(i-1,j) + g_n(i+1,j)
    end foreach

    """
    Then it should expand into
    """
    call patch_iterator%get_field(f_data, f, info)
    call patch_iterator%get_field(g_data, g, info)

    i = 1
    do j = 1, patch_iterator%nnodes(2)
      f_data(i,j,1) = g_data(i+1,j)
    ENDDO

    do i = 2, patch_iterator%nnodes(1) - 1
      do j = 1, patch_iterator%nnodes(2)
        f_data(i,j,1) = g_data(i-1,j) + g_data(i+1,j)
      ENDDO
    ENDDO

    i = patch_iterator%nnodes(1)
    do j = 1, patch_iterator%nnodes(2)
      f_data(i,j,1) = g_data(i-1,j)
      f_data(i,j,2) = g_data(i,j)
      f_data(i,j,3) = g_data(i+1,j)
    ENDDO

    """
