import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import CookieProblem from '../components/CookieProblem'

describe('CookieProblem Component', () => {
  it('renders the component with initial values', () => {
    render(<CookieProblem />)
    
    expect(screen.getByText('Cookie Problem')).toBeInTheDocument()
    expect(screen.getByDisplayValue('30')).toBeInTheDocument() // Bowl 1 vanilla
    expect(screen.getByDisplayValue('10')).toBeInTheDocument() // Bowl 1 chocolate
    expect(screen.getByDisplayValue('20')).toBeInTheDocument() // Bowl 2 vanilla
    expect(screen.getByDisplayValue('20')).toBeInTheDocument() // Bowl 2 chocolate
  })

  it('calculates posterior probability correctly', () => {
    render(<CookieProblem />)
    
    // With initial values: 
    // P(Bowl 1 | vanilla) = (0.5 * 30/40) / (0.5 * 30/40 + 0.5 * 20/40) = 0.6
    expect(screen.getByText('60.00%')).toBeInTheDocument()
  })

  it('updates calculation when bowl composition changes', () => {
    render(<CookieProblem />)
    
    const bowl1Vanilla = screen.getByLabelText('Bowl 1 Vanilla Cookies:')
    fireEvent.change(bowl1Vanilla, { target: { value: '40' } })
    
    // With 40 vanilla in bowl 1: (0.5 * 40/50) / (0.5 * 40/50 + 0.5 * 20/40) = 0.615
    expect(screen.getByText('61.54%')).toBeInTheDocument()
  })

  it('handles zero cookies edge case', () => {
    render(<CookieProblem />)
    
    const bowl1Vanilla = screen.getByLabelText('Bowl 1 Vanilla Cookies:')
    const bowl1Chocolate = screen.getByLabelText('Bowl 1 Chocolate Cookies:')
    
    fireEvent.change(bowl1Vanilla, { target: { value: '0' } })
    fireEvent.change(bowl1Chocolate, { target: { value: '1' } })
    
    expect(screen.getByText('0.00%')).toBeInTheDocument()
  })

  it('displays step-by-step calculation', () => {
    render(<CookieProblem />)
    
    expect(screen.getByText(/Step 1:/)).toBeInTheDocument()
    expect(screen.getByText(/Step 2:/)).toBeInTheDocument()
    expect(screen.getByText(/Step 3:/)).toBeInTheDocument()
    expect(screen.getByText(/Step 4:/)).toBeInTheDocument()
  })

  it('shows interpretation of results', () => {
    render(<CookieProblem />)
    
    expect(screen.getByText(/Interpretation/)).toBeInTheDocument()
    expect(screen.getByText(/Given that we drew a vanilla cookie/)).toBeInTheDocument()
  })

  test('all inputs have proper validation', () => {
    render(<CookieProblem />)
    
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      expect(input).toHaveAttribute('min', '0')
      expect(input).toHaveAttribute('max', '100')
    })
  })

  test('probability calculations are accurate', () => {
    render(<CookieProblem />)
    
    // Test specific calculation
    // Prior: 0.5 for each bowl
    // Likelihood: 30/40 for bowl 1, 20/40 for bowl 2
    // Posterior: (0.5 * 0.75) / (0.5 * 0.75 + 0.5 * 0.5) = 0.6
    
    expect(screen.getByText('75.00%')).toBeInTheDocument() // Likelihood for Bowl 1
    expect(screen.getByText('50.00%')).toBeInTheDocument() // Likelihood for Bowl 2
    expect(screen.getByText('60.00%')).toBeInTheDocument() // Posterior
  })

  test('handles equal probability case', () => {
    render(<CookieProblem />)
    
    // Set up equal vanilla cookies in both bowls
    const bowl1Vanilla = screen.getByLabelText('Bowl 1 Vanilla Cookies:')
    const bowl2Vanilla = screen.getByLabelText('Bowl 2 Vanilla Cookies:')
    
    fireEvent.change(bowl1Vanilla, { target: { value: '25' } })
    fireEvent.change(bowl2Vanilla, { target: { value: '25' } })
    
    expect(screen.getByText('50.00%')).toBeInTheDocument()
  })

  test('displays mathematical notation correctly', () => {
    render(<CookieProblem />)
    
    expect(screen.getByText(/P\(Bowl 1 \| vanilla\)/)).toBeInTheDocument()
    expect(screen.getByText(/P\(vanilla \| Bowl 1\)/)).toBeInTheDocument()
  })
})
