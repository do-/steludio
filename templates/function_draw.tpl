	return

		draw_table (

			array (
				'������������',
			),

			create_function ('$i', '

				return draw_cells (array (
					href => "/?type=__TYPE__&id=$i[id]",
				), array (
	
					$i -> {label},

					array (
						type => "checkbox",
						name => "___TYPE___$i[id]",
					),

				))

			'),

			$data [__TYPE__],

			{
			
				title => array (label => ...),

				top_toolbar => array (array (
						keep_params => array ('type', 'select'),
					),
					array (
						icon  => 'create',
						label => '&��������',
						href  => '?type=__TYPE__&action=create',
					),

					array (
						type  => 'input_text',
						label => '������',
						name  => 'q',
						keep_params => array (),
					),

					array (
						type    => 'pager',
						cnt     => count ($data [__TYPE__]),
						total   => $data [cnt],
						portion => $data [portion],
					),

					fake_select (),

				],
				
				toolbar => draw_centered_toolbar (array (), array (
					array (
						icon    => 'ok',
						label   => '����� ����������',
						href    => 'javaScript:document.form.submit()',
						confirm => '�� �������, ���?..',
					),
				)),
			}
		);
