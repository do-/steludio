	my ($data) = @_;

	draw_form ({
			right_buttons => [del ($data)],
		},
		
		$data,
		
		[
			{
				name    => 'label',
				label   => '��������',
				size    => 40,
				max_len => 255,
			},
#			{
#				name   => 'id_user',
#				label  => '������������',
#				type   => 'select',
#				values => $data -> {users},
#				empty  => '[�������� ������������]',
#				other  => '/?type=users',
#			},
		],
	)

	.

	draw_table (

		sub {
		
			draw_cells ({
				bgcolor => $i -> {id} == $data -> {id} ? '#ffffd0' : undef,
			},[
				{
					type => 'checkbox',
					name => "_clone_$$i{id}",
					off  => $i -> {id} == $data -> {id} || $i -> {id} == 1,
				},
				
				$i -> {label},
				
			]),
		
		},
		
		$data -> {clones},
		
		{
			
			title => {label => '������� ��������'},
			
			off   => !$_REQUEST{__read_only} || @{$data -> {clones}} < 2,
			
			name  => 't1',
						
			top_toolbar => [{
				keep_params => ['type', 'id'],
			},
				{
					name  => 'first',
					type  => 'input_text',
					label => '�� �������� ������',
					size  => 2,
					keep_params => [],
				},
			],
			
			toolbar => draw_centered_toolbar ({},
				
				[
					{
						icon  => 'delete',
						label => '����� ���������� ������ � �������',
						href  => "javaScript:if(confirm('�� �������, ��� ��� ���������� ������ ��������� �� ������ � �������?')) document.forms ['t1'].submit()",
					}
				]

			),

		}

	);
