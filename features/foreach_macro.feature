Feature: Foreach Macros

  In order to abstract away different looping strategies we need to
  provide a looping macro.

  Scenario: basic foreach macro
    Given a foreach macro named "particles" with argument list (particle_set)
    And body
    """
    do <%= iter %>=1,<%= particle_set %>%Npart
    <%= indent(body,2) -%>
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
  
  Scenario: particle foreach macro
    Given a foreach macro named "particles" with argument list (pset)
    And body
    """
    modifier positions(x)
    modifier fields(*fields)
    % unless x.nil?
    call P%get_xp(<%= "#{x}_#{iter}" %>,info)
    % end
    % fields.each do |f|
    call P%get_field(<%= f[1] %>,<%= "#{f[0]}_#{iter}" %>,info)
    % end
    do <%= iter %>=1,<%= pset %>%Npart
    <%= indent(body,2) -%>
    <%= transform body, "w_p", "w_p($1,#{iter})" %>
    end do
    end macro

    """
    And a foreach macro named "neighbors" with argument list (pset)
    And body
    """
    do <%= iter %>=1,<%= particle_set %>%Npart
    <%= indent(body,2) -%>
    end do

    """
    When I preprocess
    """
      ! leading comment
      foreach p in particles(P) with positions(x) fields(w=weight)
        foreach q in neighbors(p)
          ! applying a kernel eta to all w_p
          w_p = w_p + sqrt(sum((x_p(:) - x_q(:))**2)) * eta(w_p,w_q)
        end foreach
      end foreach

    """
    Then it should expand into
    """
      ! leading comment
      call P%get_xp(x_p,info)
      call P%get_field(weight,w_p,info)
      nlist => P%get_neighlist(P)
      do p=1,P%Npart
        do q=1,nlist%nvlist(p)
          ! applying a kernel eta to all vp
          w_p(p) = w_p(p) + sqrt(sum((x_p(:,p) - x_q(:,q))**2)) * eta(w_p(p),w_q(q))
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
    <%= indent(body, body_indent) -%>
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
    %   transform body, "#{f}_#{iter}", "#{f}_data(#{i},#{j},$1)"
    % end
      do <%= i %> = 1, patch_iterator%nnodes(1)
        do <%= j %> = 1, patch_iterator%nnodes(2)
    <%= indent(body, 6) -%>
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
    
