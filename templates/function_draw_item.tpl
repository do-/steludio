	draw_form (array (
			right_buttons => array (del ($data)),
		),
		
		$data,
		
		array (
			array (
				name  => 'label',
				label => 'Название',
				size  => 40,
			),
		),
	)

	.

	draw_table (

		create_function ('$i', '
		
			draw_cells (array (
				bgcolor => $i [id] == $data [id] ? '#ffffd0' : undef,
			), array (
			
				array (
					type => 'checkbox',
					name => "_clone_$i[id]",
					off  => $i [id] == $data [id] || $i [id] == 1,
				),
				
				$i [label],
				
			)),
		
		'),
		
		$data [clones],
		
		array (
			
			title => array (label => 'Похожие названия'),
			
			off   => !$_REQUEST [__read_only] || count ($data [clones]) < 2,
			
			name  => 't1',
						
			top_toolbar => array (array (
				keep_params => array ('type', 'id'),
			),
				array (
					name  => 'first',
					type  => 'input_text',
					label => 'По скольким буквам',
					size  => 2,
					keep_params => array (),
				),
			),
			
			toolbar => draw_centered_toolbar (array (),
				
				array (
					array (
						icon  => 'delete',
						label => 'слить выделенные записи с текущей',
						href  => "javaScript:if(confirm(Вы уверены, что все выделенные записи совпадают по смыслу с текущей?')) document.forms[t1].submit()",
					),
				),

			),

		)

	);
