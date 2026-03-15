// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import react from '@astrojs/react';
import tailwindcss from '@tailwindcss/vite';

// https://astro.build/config
export default defineConfig({
	site: 'https://petradb.dev',
	vite: {
		plugins: [tailwindcss({ optimize: false })],
		ssr: {
			external: [
				'@codemirror/autocomplete', '@codemirror/commands',
				'@codemirror/lang-css', '@codemirror/lang-html',
				'@codemirror/lang-javascript', '@codemirror/lang-json',
				'@codemirror/lang-markdown', '@codemirror/lang-python',
				'@codemirror/lang-sql', '@codemirror/lang-xml',
				'@codemirror/language', '@codemirror/lint',
				'@codemirror/search', '@codemirror/state', '@codemirror/view',
			],
		},
	},
	integrations: [
		react({
			include: ['**/components/**/*.tsx'],
		}),
		starlight({
			title: 'PetraDB',
			social: [
				{ icon: 'github', label: 'GitHub', href: 'https://github.com/edadma/petradb' },
				{ icon: 'npm', label: 'npm', href: 'https://www.npmjs.com/package/@petradb/engine' },
			],
			customCss: ['./src/styles/custom.css'],
			head: [
				{
					tag: 'script',
					content: `(()=>{if(location.pathname.replace(/\\/$/,'').endsWith('/playground'))document.documentElement.classList.add('playground-page')})()`,
				},
			],
			sidebar: [
				{ label: 'Changelog', slug: 'changelog' },
				{
					label: 'Getting Started',
					items: [
						{ label: 'JavaScript / TypeScript', slug: 'getting-started/javascript' },
						{ label: 'Scala', slug: 'getting-started/scala' },
						{ label: 'Java', slug: 'getting-started/java' },
						{ label: 'C / Native', slug: 'getting-started/c' },
						{ label: 'Playground', link: '/playground/' },
					],
				},
				{
					label: 'Guides',
					items: [
						{ label: 'JavaScript / TypeScript', slug: 'guides/javascript' },
						{ label: 'Scala', slug: 'guides/scala' },
						{ label: 'CLI', slug: 'guides/cli' },
						{ label: 'Server', slug: 'guides/server' },
						{ label: 'Client', slug: 'guides/client' },
					],
				},
				{
					label: 'Integrations',
					items: [
						{ label: 'Drizzle ORM', slug: 'integrations/drizzle' },
						{ label: 'GraphQL', slug: 'integrations/graphql' },
						{ label: 'Knex.js', slug: 'integrations/knex' },
						{ label: 'Lucid ORM', slug: 'integrations/lucid' },
						{ label: 'JDBC', slug: 'integrations/jdbc' },
						{ label: 'Quarry', slug: 'integrations/quarry' },
					],
				},
				{
					label: 'SQL Reference',
					items: [
						{ label: 'Data Types', slug: 'reference/data-types' },
						{ label: 'DDL', slug: 'reference/ddl' },
						{ label: 'DML', slug: 'reference/dml' },
						{ label: 'Queries', slug: 'reference/queries' },
						{ label: 'Functions', slug: 'reference/functions' },
						{ label: 'JSON', slug: 'reference/json' },
						{ label: 'Transactions', slug: 'reference/transactions' },
						{ label: 'PL/pgSQL', slug: 'reference/plpgsql' },
					],
				},
				{
					label: 'API Reference',
					items: [
						{ label: 'JavaScript / TypeScript', slug: 'reference/api-javascript' },
						{ label: 'Scala', slug: 'reference/api-scala' },
						{ label: 'C', slug: 'reference/api-c' },
					],
				},
			],
		}),
	],
});
