	my ($data) = @_;

	return

		draw_table (

			[
				'������������',
			],

			sub {

				draw_cells ({
					href => "/?type=__TYPE__&id=$$i{id}",
				},[
	
					$i -> {label},

					{
						type => "checkbox",
						name => "___TYPE___$$i{id}",
					},

				])

			},

			$data -> {__TYPE__},

			{
				title => {label => '...'},

				top_toolbar => [{
						keep_params => ['type', 'select'],
					},
					{
						icon  => 'create',
						label => '&��������',
						href  => '?type=__TYPE__&action=create',
					},

					{
						type  => 'input_text',
						label => '������',
						name  => 'q',
						keep_params => [],
					},

					{
						type    => 'pager',
					},

					fake_select (),

				],
				
				toolbar => draw_centered_toolbar ({},[
					{
						icon    => 'ok',
						label   => '����� ����������',
						href    => 'javaScript:document.form.submit()',
						confirm => '�� �������, ���?..',
					},
				]),
			}
		);
