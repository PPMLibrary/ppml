Feature: Foreach Macros

  In order to abstract away different looping strategies we need to
  provide a looping macro.

  Scenario: basic foreach macro
    Given a foreach macro named "particles"
    And argument list (particle_set)
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

  Scenario: modifiers
    Given setting ppm.dim is 2
    And a foreach macro named "mesh"
    And argument list (m)
    And modifier "fields" with argument list (*fields)
    And modifier "indices" with argument list (i,j,k=nil)
    And body
    """
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
    foreach node in mesh(M).fields(f1).indices(i,j)
      node%f1(1) = cos(i*h(1)+j)
      node%f2(2) = sin(i*h(1)+j)
      node%f3(3) = cos(i*h(1)+j)**2
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
          node%f2(2) = sin(i*h(1)+j)
          node%f3(3) = cos(i*h(1)+j)**2
        end do
      end do
      patch_iterator => M%subpatch%next()
    end do

    """

  Scenario: identifier manipulation
    Given a foreach macro named "mesh"
    And argument list (m)
    And modifier "indices" with argument list (i=i,j=j,k=k)
    And modifier "fields" with argument list (*fs)
    And body
    """
    patch_iterator = <%= m %>%subpatch%begin()
    do while (associated(patch_iterator))
    % fs.each_with_index do |f,ind|
      call patch_iterator%get_field(field_data<%= ind %>, <%= f %>, info)
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
    foreach node in mesh(M).fields(f1)
      node%f1(1) = cos(i*h(1)+j)
      node%f2(2) = sin(i*h(1)+j)
      node%f3(3) = cos(i*h(1)+j)**2
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
          node%f2(2) = sin(i*h(1)+j)
          node%f3(3) = cos(i*h(1)+j)**2
        end do
      end do
      patch_iterator => M%subpatch%next()
    end do

    """

  Scenario: multiple bodies
    
