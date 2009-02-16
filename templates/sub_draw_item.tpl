	my ($data) = @_;

	$_REQUEST {__focused_input} = '_label';

	draw_form ({
	
			right_buttons => [del ($data)],
			
			no_edit => $data -> {no_del},
			
			path => [
				{type => '__TYPE__', name => action_type_label},
				{type => '__TYPE__', name => $data -> {label}, id => $data -> {id}},
			],
			
		},
		
		$data,
		
		[
			{
				name    => 'label',
				label   => '������������',
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

#			{
#				name   => 'id_org',
#				label  => '�����������',
#				type   => 'suggest',
#				size    => 40,
#				values => sub {sql (orgs => ['id',
#					['label LIKE ?%' => $_REQUEST {_id_org}],
#				])},
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
					off  => $i -> {id} == $data -> {id},
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
