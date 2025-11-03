import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import DiceProblem from '../components/DiceProblem'

describe('DiceProblem Component', () => {
  it('renders the component with initial 6-sided die', () => {
    render(<DiceProblem />)
    
    expect(screen.getByText('Dice Problem')).toBeInTheDocument()
    expect(screen.getByDisplayValue('6')).toBeInTheDocument() // Number of sides
    expect(screen.getByDisplayValue('1')).toBeInTheDocument() // Target number
  })

  it('calculates probability correctly for 6-sided die', () => {
    render(<DiceProblem />)
    
    // For a 6-sided die, probability of rolling 1 is 1/6 = 16.67%
    expect(screen.getByText('16.67%')).toBeInTheDocument()
  })

  it('updates calculation when number of sides changes', () => {
    render(<DiceProblem />)
    
    const sidesInput = screen.getByLabelText('Number of Sides:')
    fireEvent.change(sidesInput, { target: { value: '20' } })
    
    // For a 20-sided die, probability of rolling 1 is 1/20 = 5%
    expect(screen.getByText('5.00%')).toBeInTheDocument()
  })

  it('updates calculation when target number changes', () => {
    render(<DiceProblem />)
    
    const targetInput = screen.getByLabelText('Target Number:')
    fireEvent.change(targetInput, { target: { value: '3' } })
    
    // For a 6-sided die, probability of rolling 3 is 1/6 = 16.67%
    expect(screen.getByText('16.67%')).toBeInTheDocument()
  })

  it('handles invalid target number (greater than sides)', () => {
    render(<DiceProblem />)
    
    const sidesInput = screen.getByLabelText('Number of Sides:')
    const targetInput = screen.getByLabelText('Target Number:')
    
    fireEvent.change(sidesInput, { target: { value: '4' } })
    fireEvent.change(targetInput, { target: { value: '6' } })
    
    // Should show error or 0% probability
    expect(screen.getByText('0.00%')).toBeInTheDocument()
  })

  it('displays probability distribution table', () => {
    render(<DiceProblem />)
    
    expect(screen.getByText('Probability Distribution')).toBeInTheDocument()
    expect(screen.getByText('1')).toBeInTheDocument()
    expect(screen.getByText('2')).toBeInTheDocument()
    expect(screen.getByText('6')).toBeInTheDocument()
  })

  it('shows step-by-step calculation', () => {
    render(<DiceProblem />)
    
    expect(screen.getByText(/Step 1:/)).toBeInTheDocument()
    expect(screen.getByText(/Step 2:/)).toBeInTheDocument()
    expect(screen.getByText(/Step 3:/)).toBeInTheDocument()
  })

  it('highlights the target number in the distribution', () => {
    render(<DiceProblem />)
    
    // The target number (1) should be highlighted in the table
    const targetRow = screen.getByText('1').closest('tr')
    expect(targetRow).toHaveClass('bg-blue-50')
  })

  test('handles minimum valid input', () => {
    render(<DiceProblem />)
    
    const sidesInput = screen.getByLabelText('Number of Sides:')
    fireEvent.change(sidesInput, { target: { value: '2' } })
    
    // For a 2-sided die, probability is 1/2 = 50%
    expect(screen.getByText('50.00%')).toBeInTheDocument()
  })

  test('handles large number of sides', () => {
    render(<DiceProblem />)
    
    const sidesInput = screen.getByLabelText('Number of Sides:')
    fireEvent.change(sidesInput, { target: { value: '100' } })
    
    // For a 100-sided die, probability is 1/100 = 1%
    expect(screen.getByText('1.00%')).toBeInTheDocument()
  })

  test('displays mathematical formula correctly', () => {
    render(<DiceProblem />)
    
    expect(screen.getByText(/P\(Target\) = 1 \/ Number of Sides/)).toBeInTheDocument()
  })

  test('all interactive elements are accessible', () => {
    render(<DiceProblem />)
    
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      expect(input).toHaveAttribute('aria-label')
      expect(input).toHaveAttribute('min')
    })
  })

  test('shows interpretation of results', () => {
    render(<DiceProblem />)
    
    expect(screen.getByText(/Interpretation/)).toBeInTheDocument()
    expect(screen.getByText(/For a fair 6-sided die/)).toBeInTheDocument()
  })
})
